import "./App.css";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import initWasm, { Sheet } from "../pkg/sheetlang";
import { getTerminalHref, getTerminalHrefWithCode } from "./routing";

type CommandResult =
	| { type: "output"; text: string }
	| { type: "bshow"; coords: string[] }
	| { type: "demo"; script: string }
	| { type: "batch"; results: CommandResult[] }
	| { type: "error"; message: string; span_start?: number; span_end?: number }
	| { type: "exit" };

type UiTensorSummary = {
	name: string;
	shape: number[];
	formula_count: number;
	input_count: number;
	state_count: number;
	is_active: boolean;
};

type UiSnapshot = {
	active_tensor: string;
	dims: number;
	view_axes: [number, number];
	view_offset: number[];
	pending_effects: number;
	effect_auto_budget: number;
	tensors: UiTensorSummary[];
};

type UiCell = {
	col: number;
	row: number;
	view_label: string;
	tensor_coord: number[];
	tensor_token: string;
	value: string | null;
	formula: string | null;
	input: string | null;
	has_formula: boolean;
	has_input: boolean;
	emits_effect: boolean;
};

type UiCellDetail = {
	col: number;
	row: number;
	view_label: string;
	tensor_coord: number[];
	tensor_token: string;
	value: string | null;
	formula: string | null;
	input: string | null;
	has_formula: boolean;
	has_input: boolean;
	emits_effect: boolean;
	from_input: boolean;
	dependencies: string[];
	dependents: string[];
	error: string | null;
};

type UiStateEntry = {
	coord: string;
	value: string;
};

type LogEntry = {
	id: number;
	at: number;
	kind: "cmd" | "out" | "error" | "info";
	text: string;
};

type TickEntry = {
	index: number;
	at: number;
	command: string;
	duration_ms: number;
	changed_cells: number;
	enqueued_effects: number;
	pending_effects: number;
	snapshot: Record<string, string>;
};

type EffectRunEntry = {
	id: number;
	at: number;
	command: string;
	summary: string;
	pending_after: number;
};

type DemoEntry = {
	number: number;
	name: string;
};

type SelectionPoint = {
	col: number;
	row: number;
};

type AxisSliceMode = "single" | "span" | "full" | "prefix" | "suffix";

const GRID_COLS = 12;
const GRID_ROWS = 16;

const toTimeLabel = (ts: number) => new Date(ts).toLocaleTimeString();

const parseStateMap = (raw: unknown): Record<string, string> => {
	if (!Array.isArray(raw)) {
		return {};
	}
	const map: Record<string, string> = {};
	for (const item of raw) {
		if (!item || typeof item !== "object") {
			continue;
		}
		const entry = item as UiStateEntry;
		if (typeof entry.coord === "string" && typeof entry.value === "string") {
			map[entry.coord] = entry.value;
		}
	}
	return map;
};

const diffTokens = (
	previous: Record<string, string> | null,
	next: Record<string, string>,
): string[] => {
	if (!previous) {
		return Object.keys(next);
	}
	const all = new Set<string>([...Object.keys(previous), ...Object.keys(next)]);
	const changed: string[] = [];
	for (const token of all) {
		if (previous[token] !== next[token]) {
			changed.push(token);
		}
	}
	return changed;
};

const toA1 = (col: number, row: number): string | null => {
	if (col < 0 || col > 25 || row < 0) {
		return null;
	}
	return `${String.fromCharCode(65 + col)}${row + 1}`;
};

const toA1Range = (start: SelectionPoint, end: SelectionPoint): string | null => {
	const minCol = Math.min(start.col, end.col);
	const maxCol = Math.max(start.col, end.col);
	const minRow = Math.min(start.row, end.row);
	const maxRow = Math.max(start.row, end.row);
	const startA1 = toA1(minCol, minRow);
	const endA1 = toA1(maxCol, maxRow);
	if (!startA1 || !endA1) {
		return null;
	}
	return `${startA1}:${endA1}`;
};

const buildSelectionToken = (start: SelectionPoint, end: SelectionPoint) => {
	const minCol = Math.min(start.col, end.col);
	const maxCol = Math.max(start.col, end.col);
	const minRow = Math.min(start.row, end.row);
	const maxRow = Math.max(start.row, end.row);

	const colSpec = minCol === maxCol ? `${minCol}` : `${minCol}:${maxCol}`;
	const rowSpec = minRow === maxRow ? `${minRow}` : `${minRow}:${maxRow}`;
	return `#[${colSpec},${rowSpec}]`;
};

const parseIntList = (value: string): number[] | null => {
	const parts = value
		.split(",")
		.map((part) => part.trim())
		.filter((part) => part.length > 0);

	if (parts.length === 0) {
		return [];
	}

	const numbers: number[] = [];
	for (const part of parts) {
		const n = Number(part);
		if (!Number.isInteger(n)) {
			return null;
		}
		numbers.push(n);
	}
	return numbers;
};

const parseDemoEntries = (lines: string[]): DemoEntry[] => {
	const entries: DemoEntry[] = [];
	for (const line of lines) {
		const match = line.match(/^\s*(\d+)\.\s+(.+)$/);
		if (!match) {
			continue;
		}
		entries.push({
			number: Number(match[1]),
			name: match[2].trim(),
		});
	}
	return entries;
};

const parseCountFromOutputs = (lines: string[], prefix: string): number => {
	for (const line of lines) {
		if (!line.startsWith(prefix)) {
			continue;
		}
		const value = Number(line.slice(prefix.length).trim());
		if (Number.isFinite(value)) {
			return value;
		}
	}
	return 0;
};

const parseTensorToken = (token: string): { tensor: string | null; coords: number[] } | null => {
	const match = token.match(/^([A-Za-z_][A-Za-z0-9_]*)?#\[([^\]]+)\]$/);
	if (!match) {
		return null;
	}
	const coords = match[2]
		.split(",")
		.map((part) => Number(part.trim()));
	if (coords.some((value) => !Number.isInteger(value))) {
		return null;
	}
	return {
		tensor: match[1] ?? null,
		coords,
	};
};

const axisSpec = (mode: AxisSliceMode, start: number, end: number) => {
	switch (mode) {
		case "full":
			return ":";
		case "prefix":
			return `:${end}`;
		case "suffix":
			return `${start}:`;
		case "span":
			return `${start}:${end}`;
		case "single":
		default:
			return `${start}`;
	}
};

const shapeLabel = (shape: number[]) => {
	if (shape.length === 0) {
		return "scalar";
	}
	return `[${shape.join(",")}]`;
};

const isTickLike = (command: string) => {
	const trimmed = command.trim();
	return trimmed.startsWith("tick") || trimmed.startsWith("effects run");
};

function StudioApp() {
	const [sheet, setSheet] = useState<Sheet | null>(null);
	const [ready, setReady] = useState(false);
	const [snapshot, setSnapshot] = useState<UiSnapshot | null>(null);
	const [cells, setCells] = useState<UiCell[]>([]);
	const [inspector, setInspector] = useState<UiCellDetail | null>(null);

	const [viewportCol, setViewportCol] = useState(0);
	const [viewportRow, setViewportRow] = useState(0);

	const [selectionAnchor, setSelectionAnchor] = useState<SelectionPoint | null>(null);
	const [selectionFocus, setSelectionFocus] = useState<SelectionPoint | null>(null);
	const [dragging, setDragging] = useState(false);

	const [showExcelLabels, setShowExcelLabels] = useState(false);
	const [editMode, setEditMode] = useState<"formula" | "state" | "command">("formula");
	const [editorValue, setEditorValue] = useState("");
	const [targetTokenInput, setTargetTokenInput] = useState("");

	const [commandInput, setCommandInput] = useState("");
	const [tickCount, setTickCount] = useState(1);
	const [showDemoBrowser, setShowDemoBrowser] = useState(false);

	const [newTensorName, setNewTensorName] = useState("grid");
	const [newTensorShape, setNewTensorShape] = useState("32,32");

	const [viewAxesDraft, setViewAxesDraft] = useState<[number, number]>([0, 1]);
	const [viewOffsetDraft, setViewOffsetDraft] = useState("0,0");

	const [autoBudgetDraft, setAutoBudgetDraft] = useState("0");

	const [xMode, setXMode] = useState<AxisSliceMode>("single");
	const [xStart, setXStart] = useState(0);
	const [xEnd, setXEnd] = useState(3);
	const [yMode, setYMode] = useState<AxisSliceMode>("single");
	const [yStart, setYStart] = useState(0);
	const [yEnd, setYEnd] = useState(3);
	const [useBuilderTarget, setUseBuilderTarget] = useState(false);

	const [logs, setLogs] = useState<LogEntry[]>([]);
	const [commandHistory, setCommandHistory] = useState<string[]>([]);
	const [timeline, setTimeline] = useState<TickEntry[]>([]);
	const [selectedTickIndex, setSelectedTickIndex] = useState<number | null>(null);
	const [recentChanged, setRecentChanged] = useState<Set<string>>(new Set());
	const [effectRuns, setEffectRuns] = useState<EffectRunEntry[]>([]);
	const [demoEntries, setDemoEntries] = useState<DemoEntry[]>([]);

	const logIdRef = useRef(1);
	const effectIdRef = useRef(1);
	const timelineRef = useRef<TickEntry[]>([]);

	useEffect(() => {
		timelineRef.current = timeline;
	}, [timeline]);

	const addLog = useCallback((kind: LogEntry["kind"], text: string) => {
		setLogs((prev) => {
			const next: LogEntry = {
				id: logIdRef.current,
				at: Date.now(),
				kind,
				text,
			};
			logIdRef.current += 1;
			const merged = [...prev, next];
			return merged.slice(-500);
		});
	}, []);

	const selectedCell = selectionFocus ?? selectionAnchor;
	const selectionToken = useMemo(() => {
		if (!selectionAnchor || !selectionFocus) {
			return "";
		}
		return buildSelectionToken(selectionAnchor, selectionFocus);
	}, [selectionAnchor, selectionFocus]);

	const builderToken = useMemo(() => {
		const x = axisSpec(xMode, xStart, xEnd);
		const y = axisSpec(yMode, yStart, yEnd);
		return `#[${x},${y}]`;
	}, [xMode, xStart, xEnd, yMode, yStart, yEnd]);

	const effectiveTargetToken = useMemo(() => {
		if (useBuilderTarget) {
			return builderToken;
		}
		const manual = targetTokenInput.trim();
		if (manual.length > 0) {
			return manual;
		}
		if (selectionToken.length > 0) {
			return selectionToken;
		}
		return "#[0,0]";
	}, [builderToken, selectionToken, targetTokenInput, useBuilderTarget]);

	const readOnlyTick = useMemo(() => {
		if (selectedTickIndex == null) {
			return null;
		}
		return timeline.find((entry) => entry.index === selectedTickIndex) ?? null;
	}, [selectedTickIndex, timeline]);

	const selectedTickDiff = useMemo(() => {
		if (!readOnlyTick) {
			return new Set<string>();
		}
		const prev = timeline.find((entry) => entry.index === readOnlyTick.index - 1) ?? null;
		const changed = diffTokens(prev?.snapshot ?? null, readOnlyTick.snapshot);
		return new Set(changed);
	}, [readOnlyTick, timeline]);

	const cellMap = useMemo(() => {
		const map = new Map<string, UiCell>();
		for (const cell of cells) {
			map.set(`${cell.col}:${cell.row}`, cell);
		}
		return map;
	}, [cells]);

	const refreshUi = useCallback(
		(engine: Sheet, selected: SelectionPoint | null): UiSnapshot | null => {
			const nextSnapshot = (engine.ui_snapshot?.() as UiSnapshot | undefined) ?? null;
			if (nextSnapshot) {
				setSnapshot(nextSnapshot);
				setViewAxesDraft(nextSnapshot.view_axes);
				setViewOffsetDraft(nextSnapshot.view_offset.join(","));
				setAutoBudgetDraft(String(nextSnapshot.effect_auto_budget));
			}

			const rawCells = engine.ui_cells?.(viewportCol, viewportRow, GRID_COLS, GRID_ROWS);
			if (Array.isArray(rawCells)) {
				setCells(rawCells as UiCell[]);
			} else {
				setCells([]);
			}

			if (selected) {
				const detail = engine.ui_inspect_view_cell?.(selected.col, selected.row);
				setInspector((detail as UiCellDetail | undefined) ?? null);
			} else {
				setInspector(null);
			}

			return nextSnapshot;
		},
		[viewportCol, viewportRow],
	);

	const captureTick = useCallback(
		(
			command: string,
			durationMs: number,
			outputLines: string[],
			nextSnapshot: UiSnapshot | null,
			engine: Sheet,
		) => {
			const current = parseStateMap(engine.ui_active_state?.() as unknown);
			const previous = timelineRef.current.length > 0
				? timelineRef.current[timelineRef.current.length - 1].snapshot
				: null;
			const changed = diffTokens(previous, current);
			setRecentChanged(new Set(changed));
			const enqueued = parseCountFromOutputs(outputLines, "Effects queued:");
			const pending = nextSnapshot?.pending_effects ?? parseCountFromOutputs(outputLines, "Pending effects:");

			setTimeline((prev) => {
				const entry: TickEntry = {
					index: prev.length + 1,
					at: Date.now(),
					command,
					duration_ms: durationMs,
					changed_cells: changed.length,
					enqueued_effects: enqueued,
					pending_effects: pending,
					snapshot: current,
				};
				return [...prev.slice(-199), entry];
			});
			setSelectedTickIndex(null);
		},
		[],
	);

	const executeCommand = useCallback(
		(
			rawInput: string,
			options: { recordHistory?: boolean; logCommand?: boolean } = {},
		): { outputs: string[]; errors: string[]; snapshot: UiSnapshot | null } | null => {
			if (!sheet) {
				addLog("error", "WASM engine is not ready yet.");
				return null;
			}

			const input = rawInput.trim();
			if (!input) {
				return null;
			}

			const shouldRecordHistory = options.recordHistory !== false;
			const shouldLogCommand = options.logCommand !== false;

			const executeOne = (command: string): { outputs: string[]; errors: string[]; snapshot: UiSnapshot | null } => {
				if (shouldLogCommand) {
					addLog("cmd", `$ ${command}`);
				}
				if (shouldRecordHistory) {
					setCommandHistory((prev) => [...prev, command]);
				}

				const started = performance.now();
				const result = sheet.execute_command?.(command) as CommandResult | undefined;
				const outputs: string[] = [];
				const errors: string[] = [];

				const processResult = (value: CommandResult) => {
					switch (value.type) {
						case "output": {
							const lines = value.text.split("\n").filter((line) => line.length > 0);
							if (lines.length === 0) {
								addLog("out", "(ok)");
							} else {
								for (const line of lines) {
									addLog("out", line);
								}
								outputs.push(...lines);
							}
							break;
						}
						case "bshow": {
							addLog("out", `bshow rendered ${value.coords.length} coord(s)`);
							outputs.push(`bshow:${value.coords.length}`);
							break;
						}
						case "error": {
							if (typeof value.span_start === "number") {
								const start = Math.max(0, value.span_start);
								const end = Math.max(start + 1, value.span_end ?? start + 1);
								const indicator = " ".repeat(2 + start) + "^".repeat(end - start);
								addLog("error", indicator);
							}
							addLog("error", `Error: ${value.message}`);
							errors.push(value.message);
							break;
						}
						case "exit": {
							addLog("info", "Exit is terminal-only in the web app.");
							break;
						}
						case "batch": {
							for (const item of value.results) {
								processResult(item);
							}
							break;
						}
						case "demo": {
							const lines = value.script
								.split(/\r?\n/)
								.map((line) => line.trim())
								.filter((line) => line.length > 0);
							addLog("info", `Running demo with ${lines.length} command(s)...`);
							for (const line of lines) {
								const nested = executeOne(line);
								outputs.push(...nested.outputs);
								errors.push(...nested.errors);
							}
							addLog("info", "Demo run completed.");
							break;
						}
					}
				};

				if (result) {
					processResult(result);
				}

				const nextSnapshot = refreshUi(sheet, selectedCell);
				const durationMs = performance.now() - started;
				if (isTickLike(command)) {
					captureTick(command, durationMs, outputs, nextSnapshot, sheet);
				}

				if (command === "demo") {
					setDemoEntries(parseDemoEntries(outputs));
				}

				if (command.startsWith("effects")) {
					setEffectRuns((prev) => {
						const summary = outputs[outputs.length - 1] ?? "effects command executed";
						const entry: EffectRunEntry = {
							id: effectIdRef.current,
							at: Date.now(),
							command,
							summary,
							pending_after: nextSnapshot?.pending_effects ?? 0,
						};
						effectIdRef.current += 1;
						return [...prev.slice(-199), entry];
					});
				}

				return { outputs, errors, snapshot: nextSnapshot };
			};

			return executeOne(input);
		},
		[addLog, captureTick, refreshUi, selectedCell, sheet],
	);

	useEffect(() => {
		let cancelled = false;

		const boot = async () => {
			try {
				await initWasm();
				if (cancelled) {
					return;
				}
				const engine = new Sheet();
				setSheet(engine);
				setReady(true);
				addLog("info", "SheetLang Studio initialized");
				refreshUi(engine, null);
			} catch (error) {
				addLog("error", `Failed to initialize WASM: ${String(error)}`);
			}
		};

		void boot();

		return () => {
			cancelled = true;
		};
	}, [addLog, refreshUi]);

	useEffect(() => {
		if (!dragging) {
			return;
		}
		const stopDrag = () => setDragging(false);
		window.addEventListener("mouseup", stopDrag);
		return () => {
			window.removeEventListener("mouseup", stopDrag);
		};
	}, [dragging]);

	useEffect(() => {
		if (!sheet) {
			return;
		}
		refreshUi(sheet, selectedCell);
	}, [sheet, viewportCol, viewportRow, selectedCell, refreshUi]);

	useEffect(() => {
		if (selectionToken.length > 0 && !useBuilderTarget && targetTokenInput.trim().length === 0) {
			setTargetTokenInput(selectionToken);
		}
	}, [selectionToken, targetTokenInput, useBuilderTarget]);

	const inspectCell = useCallback(
		(cell: SelectionPoint) => {
			if (!sheet) {
				return;
			}
			const detail = sheet.ui_inspect_view_cell?.(cell.col, cell.row);
			setInspector((detail as UiCellDetail | undefined) ?? null);
		},
		[sheet],
	);

	const selectCell = useCallback(
		(cell: SelectionPoint, extend: boolean) => {
			if (extend && selectionAnchor) {
				setSelectionFocus(cell);
				inspectCell(cell);
				return;
			}
			setSelectionAnchor(cell);
			setSelectionFocus(cell);
			inspectCell(cell);
		},
		[inspectCell, selectionAnchor],
	);

	const onCellMouseDown = useCallback(
		(cell: UiCell, shiftKey: boolean) => {
			selectCell({ col: cell.col, row: cell.row }, shiftKey);
			setDragging(true);
		},
		[selectCell],
	);

	const onCellMouseEnter = useCallback(
		(cell: UiCell) => {
			if (!dragging) {
				return;
			}
			setSelectionFocus({ col: cell.col, row: cell.row });
		},
		[dragging],
	);

	const runEditorApply = useCallback(() => {
		if (readOnlyTick) {
			addLog("info", "Leave snapshot mode to edit state or formulas.");
			return;
		}

		if (editMode === "command") {
			executeCommand(commandInput);
			setCommandInput("");
			return;
		}

		const expr = editorValue.trim();
		if (expr.length === 0) {
			addLog("error", "Editor expression is empty.");
			return;
		}

		const operator = editMode === "formula" ? "=" : ":=";
		const command = `${effectiveTargetToken} ${operator} ${expr}`;
		executeCommand(command);
	}, [
		addLog,
		commandInput,
		editMode,
		editorValue,
		effectiveTargetToken,
		executeCommand,
		readOnlyTick,
	]);

	const runTick = useCallback(() => {
		if (readOnlyTick) {
			addLog("info", "Leave snapshot mode to run ticks.");
			return;
		}
		const count = Number.isInteger(tickCount) && tickCount > 0 ? tickCount : 1;
		for (let i = 0; i < count; i += 1) {
			executeCommand("tick");
		}
	}, [addLog, executeCommand, readOnlyTick, tickCount]);

	const runTickSelection = useCallback(() => {
		if (!selectionAnchor || !selectionFocus) {
			addLog("error", "Select a range before running tick range.");
			return;
		}
		const range = toA1Range(selectionAnchor, selectionFocus);
		if (!range) {
			addLog("error", "Tick range needs view coordinates in A1..Z rows >= 1.");
			return;
		}
		executeCommand(`tick ${range}`);
	}, [addLog, executeCommand, selectionAnchor, selectionFocus]);

	const applyView = useCallback(() => {
		if (!snapshot) {
			return;
		}
		if (viewAxesDraft[0] === viewAxesDraft[1]) {
			addLog("error", "View axes must be different dimensions.");
			return;
		}
		const offset = parseIntList(viewOffsetDraft);
		if (!offset) {
			addLog("error", "View offset must be a comma-separated integer list.");
			return;
		}
		if (offset.length !== snapshot.dims) {
			addLog("error", `View offset length must match dims (${snapshot.dims}).`);
			return;
		}
		executeCommand(
			`view axes [${viewAxesDraft[0]},${viewAxesDraft[1]}] offset [${offset.join(",")}]`,
		);
	}, [addLog, executeCommand, snapshot, viewAxesDraft, viewOffsetDraft]);

	const resetView = useCallback(() => {
		if (!snapshot) {
			return;
		}
		const zeros = Array.from({ length: snapshot.dims }, () => 0);
		executeCommand(`view axes [0,1] offset [${zeros.join(",")}]`);
	}, [executeCommand, snapshot]);

	const createTensor = useCallback(() => {
		const name = newTensorName.trim();
		if (name.length === 0) {
			addLog("error", "Tensor name cannot be empty.");
			return;
		}
		const shape = parseIntList(newTensorShape);
		if (!shape || shape.length === 0) {
			addLog("error", "Shape must be comma-separated integers (example: 32,32). ");
			return;
		}
		executeCommand(`alloc ${name} [${shape.join(",")}]`);
	}, [addLog, executeCommand, newTensorName, newTensorShape]);

	const applyAutoBudget = useCallback(() => {
		const n = Number(autoBudgetDraft);
		if (!Number.isInteger(n) || n < 0) {
			addLog("error", "Auto budget must be a non-negative integer.");
			return;
		}
		executeCommand(`effects auto ${n}`);
	}, [addLog, autoBudgetDraft, executeCommand]);

	const jumpToDependency = useCallback(
		(token: string) => {
			const parsed = parseTensorToken(token);
			if (!parsed) {
				addLog("error", `Cannot parse dependency token: ${token}`);
				return;
			}

			let nextSnapshot = snapshot;
			if (parsed.tensor && snapshot && parsed.tensor !== snapshot.active_tensor) {
				const result = executeCommand(`use ${parsed.tensor}`);
				nextSnapshot = result?.snapshot ?? snapshot;
			}

			const axes = nextSnapshot?.view_axes ?? [0, 1];
			const col = parsed.coords[axes[0]] ?? 0;
			const row = parsed.coords[axes[1]] ?? 0;
			const point = { col, row };
			setSelectionAnchor(point);
			setSelectionFocus(point);
			inspectCell(point);
		},
		[addLog, executeCommand, inspectCell, snapshot],
	);

	const runDemoList = useCallback(() => {
		setShowDemoBrowser(true);
		executeCommand("demo");
	}, [executeCommand]);

	const copyAsScript = useCallback(() => {
		if (commandHistory.length === 0) {
			addLog("info", "No commands to copy.");
			return;
		}
		navigator.clipboard
			.writeText(commandHistory.join("\n"))
			.then(() => addLog("info", `Copied ${commandHistory.length} command(s) as script.`))
			.catch(() => addLog("error", "Failed to copy script to clipboard."));
	}, [addLog, commandHistory]);

	const openTerminalMode = useCallback(() => {
		const script = commandHistory.join("\n");
		window.location.href = getTerminalHrefWithCode(script);
	}, [commandHistory]);

	const clearTimeline = useCallback(() => {
		setTimeline([]);
		setSelectedTickIndex(null);
		setRecentChanged(new Set());
	}, []);

	const changedSet = readOnlyTick ? selectedTickDiff : recentChanged;

	const monoClass = "[font-family:'JetBrains_Mono','Iosevka','Cascadia_Mono',Menlo,monospace]";
	const rootClass =
		"flex h-full min-h-0 w-full flex-col bg-[radial-gradient(circle_at_15%_-10%,#243748_0,transparent_38%),radial-gradient(circle_at_100%_0%,#20303f_0,transparent_34%),#11161c] text-slate-100 [font-family:'IBM_Plex_Sans','Source_Sans_3','Trebuchet_MS',sans-serif]";
	const topbarClass =
		"z-20 flex flex-wrap items-center justify-between gap-3 border-b border-slate-700/80 bg-slate-900/85 px-3 py-3 backdrop-blur";
	const topbarGroupClass = "flex flex-wrap items-center gap-2";
	const panelClass =
		"rounded-xl border border-slate-700/70 bg-slate-900/70 shadow-[inset_0_1px_0_rgba(255,255,255,0.06)]";
	const sectionClass = `${panelClass} p-3`;
	const sectionTitleClass =
		"mb-2 text-sm font-semibold uppercase tracking-[0.07em] text-slate-300";
	const listClass = "flex min-h-0 flex-col gap-2";
	const labelClass =
		"flex min-w-0 flex-col gap-1 text-xs uppercase tracking-[0.06em] text-slate-400";
	const smallLabelClass = "min-w-16";
	const inputClass = `${monoClass} min-w-0 rounded-md border border-slate-600/70 bg-slate-900/75 px-2 py-1.5 text-sm text-slate-100 outline-none transition focus:border-cyan-400/70 focus:ring-2 focus:ring-cyan-400/35`;
	const buttonClass =
		"rounded-md border border-slate-600/70 bg-gradient-to-b from-slate-700/70 to-slate-800/90 px-2.5 py-1.5 text-sm font-medium text-slate-100 transition hover:border-cyan-400/60 hover:from-cyan-500/25 hover:to-slate-700/90 disabled:cursor-not-allowed disabled:opacity-60";
	const hintClass =
		"rounded-md border border-slate-700/80 bg-slate-900/60 px-2 py-2 text-xs italic text-slate-400";
	const topPillClass =
		"rounded-full border border-cyan-400/40 bg-cyan-500/10 px-3 py-1 text-xs font-semibold uppercase tracking-[0.05em] text-cyan-200";
	const mainClass =
		"grid min-h-0 flex-1 grid-cols-1 gap-3 p-3 lg:grid-cols-[16rem_minmax(0,1fr)] 2xl:grid-cols-[17.5rem_minmax(0,1fr)_22rem]";
	const leftPanelClass = "flex min-h-0 flex-col gap-3";
	const centerPanelClass = "flex min-h-0 flex-col gap-3";
	const rightPanelClass =
		"flex min-h-0 flex-col gap-3 lg:col-span-2 lg:grid lg:grid-cols-2 2xl:col-span-1 2xl:flex 2xl:flex-col";
	const editorBarClass = "flex flex-wrap items-center gap-2 rounded-xl border border-slate-700/75 bg-slate-900/70 p-2";
	const toolbarClass =
		"flex flex-wrap items-center justify-between gap-2 rounded-lg border border-slate-700/80 bg-slate-900/55 px-2 py-2 text-xs text-slate-300";
	const checkboxLabelClass = "inline-flex items-center gap-2 text-xs text-slate-300";
	const checkboxClass =
		"h-4 w-4 rounded border border-slate-500/80 bg-slate-900 text-cyan-400 focus:ring-2 focus:ring-cyan-400/40";
	const readonlyBannerClass =
		"flex flex-wrap items-center justify-between gap-2 rounded-lg border border-amber-400/45 bg-amber-400/10 px-3 py-2 text-sm text-amber-100";
	const gridWrapClass =
		"min-h-0 flex-1 overflow-auto rounded-xl border border-slate-700/80 bg-slate-900/75";
	const gridTableClass = `${monoClass} w-max min-w-full border-collapse text-slate-100`;
	const tableHeaderBaseClass =
		"border border-slate-700/80 bg-slate-800/90 px-2 py-1 text-xs font-semibold text-slate-300";
	const tableHeaderTopClass = `${tableHeaderBaseClass} sticky top-0 z-10`;
	const tableHeaderLeftClass = `${tableHeaderBaseClass} sticky left-0 z-[5]`;
	const tableHeaderCornerClass = `${tableHeaderBaseClass} sticky left-0 top-0 z-20`;
	const gridCellBaseClass =
		"relative h-18 w-34 cursor-cell border border-slate-700/80 bg-slate-900/55 p-1.5 align-top transition-colors hover:bg-cyan-500/10 lg:w-40";
	const gridCoordClass = `${monoClass} mb-0.5 truncate text-[10px] text-slate-400`;
	const gridValueClass = `${monoClass} min-h-8 break-words text-xs leading-snug`;
	const badgesRowClass = "mt-1 flex gap-1";
	const badgeBaseClass =
		"inline-flex rounded-sm px-1 py-0.5 text-[10px] font-semibold uppercase text-slate-950";
	const inspectorGridClass = "grid gap-1.5 text-xs";
	const inspectorRowClass =
		"grid grid-cols-[6.5rem_minmax(0,1fr)] gap-1 rounded-md border border-slate-700/80 bg-slate-900/60 px-2 py-1.5";
	const inspectorKeyClass = "uppercase tracking-[0.05em] text-slate-400";
	const sliceBuilderClass = "flex flex-col gap-2";
	const axisBlockClass =
		"grid grid-cols-1 gap-1 rounded-md border border-slate-700/80 bg-slate-900/65 p-2 md:grid-cols-[4.25rem_minmax(0,1fr)_4.25rem_4.25rem]";
	const axisLabelClass = "self-center text-xs uppercase tracking-[0.05em] text-slate-400";
	const builderTokenClass =
		`${monoClass} rounded-md border border-cyan-400/35 bg-cyan-400/10 px-2 py-1.5 text-xs text-cyan-100`;
	const bottomClass =
		"grid grid-cols-1 gap-3 border-t border-slate-700/80 bg-slate-900/80 p-3 lg:grid-cols-2 2xl:grid-cols-3";
	const bottomPaneClass = `${sectionClass} min-h-0`;
	const scrollPaneClass =
		"min-h-0 max-h-56 overflow-auto rounded-md border border-slate-700/80 bg-slate-900/60 p-2";
	const consolePaneClass = `${scrollPaneClass} ${monoClass} space-y-1`;
	const timelineControlsClass = "mb-2 flex gap-2";
	const timelineListClass = `${scrollPaneClass} space-y-2`;
	const effectsListClass = `${scrollPaneClass} space-y-2`;
	const dependencyButtonClass = `${buttonClass} w-full text-left`;
	const textureButtonClass =
		"w-full rounded-lg border border-slate-700/75 bg-slate-900/65 p-2 text-left transition hover:-translate-y-0.5 hover:border-cyan-400/45";

	return (
		<div className={rootClass}>
			<header className={topbarClass}>
				<div className={topbarGroupClass}>
					<span className="text-base font-semibold uppercase tracking-[0.12em]">SheetLang Studio</span>
					<a
						className="rounded-md bg-cyan-500/10 px-2 py-1 text-sm text-cyan-200 no-underline transition-colors hover:bg-cyan-500/20 hover:text-white"
						href={getTerminalHref()}
					>
						Open Terminal
					</a>
				</div>

				<div className={topbarGroupClass}>
					<label className={labelClass}>
						Tensor
						<select
							className={inputClass}
							value={snapshot?.active_tensor ?? ""}
							onChange={(event) => {
								const name = event.target.value;
								if (name.length > 0) {
									executeCommand(`use ${name}`);
								}
							}}
						>
							{(snapshot?.tensors ?? []).map((tensor) => (
								<option key={tensor.name} value={tensor.name}>
									{tensor.name}
								</option>
							))}
						</select>
					</label>

					<label className={`${labelClass} ${smallLabelClass}`}>
						Tick xN
						<input
							className={inputClass}
							type="number"
							value={tickCount}
							min={1}
							onChange={(event) => setTickCount(Number(event.target.value))}
						/>
					</label>
					<button className={buttonClass} type="button" onClick={runTick}>
						Tick
					</button>
					<button className={buttonClass} type="button" onClick={runTickSelection}>
						Tick Selection
					</button>
				</div>

				<div className={topbarGroupClass}>
					<span className={topPillClass}>Pending: {snapshot?.pending_effects ?? 0}</span>
					<label className={`${labelClass} ${smallLabelClass}`}>
						Auto
						<input
							className={inputClass}
							type="number"
							value={autoBudgetDraft}
							onChange={(event) => setAutoBudgetDraft(event.target.value)}
						/>
					</label>
					<button className={buttonClass} type="button" onClick={applyAutoBudget}>
						Set Auto
					</button>
					<button className={buttonClass} type="button" onClick={runDemoList}>
						Demos
					</button>
				</div>
			</header>

			<main className={mainClass}>
				<aside className={leftPanelClass}>
					<section className={sectionClass}>
						<h2 className={sectionTitleClass}>Tensors</h2>
						<div className={listClass}>
							{(snapshot?.tensors ?? []).map((tensor) => (
								<button
									type="button"
									key={tensor.name}
									className={`${textureButtonClass} ${
										tensor.is_active
											? "border-cyan-400/60 bg-cyan-500/10"
											: "border-slate-700/75"
									}`}
									onClick={() => executeCommand(`use ${tensor.name}`)}
								>
									<div className="text-sm font-semibold">{tensor.name}</div>
									<div className="mt-1 text-xs text-slate-400">
										{shapeLabel(tensor.shape)} | f:{tensor.formula_count} s:{tensor.state_count} i:{tensor.input_count}
									</div>
								</button>
							))}
						</div>
					</section>

					<section className={sectionClass}>
						<h2 className={sectionTitleClass}>Create Tensor</h2>
						<label className={labelClass}>
							Name
							<input
								className={inputClass}
								value={newTensorName}
								onChange={(event) => setNewTensorName(event.target.value)}
							/>
						</label>
						<label className={labelClass}>
							Shape
							<input
								className={inputClass}
								value={newTensorShape}
								onChange={(event) => setNewTensorShape(event.target.value)}
								placeholder="32,32"
							/>
						</label>
						<button className={buttonClass} type="button" onClick={createTensor}>
							alloc
						</button>
					</section>

					<section className={sectionClass}>
						<h2 className={sectionTitleClass}>View</h2>
						<div className="flex flex-wrap items-end gap-2">
							<label className={`${labelClass} ${smallLabelClass}`}>
								X axis
								<select
									className={inputClass}
									value={viewAxesDraft[0]}
									onChange={(event) =>
										setViewAxesDraft([Number(event.target.value), viewAxesDraft[1]])
									}
								>
									{Array.from({ length: snapshot?.dims ?? 2 }, (_, axis) => (
										<option key={`x-${axis}`} value={axis}>
											{axis}
										</option>
									))}
								</select>
							</label>

							<label className={`${labelClass} ${smallLabelClass}`}>
								Y axis
								<select
									className={inputClass}
									value={viewAxesDraft[1]}
									onChange={(event) =>
										setViewAxesDraft([viewAxesDraft[0], Number(event.target.value)])
									}
								>
									{Array.from({ length: snapshot?.dims ?? 2 }, (_, axis) => (
										<option key={`y-${axis}`} value={axis}>
											{axis}
										</option>
									))}
								</select>
							</label>
						</div>

						<label className={labelClass}>
							Offset
							<input
								className={inputClass}
								value={viewOffsetDraft}
								onChange={(event) => setViewOffsetDraft(event.target.value)}
								placeholder="0,0"
							/>
						</label>
						<div className="flex flex-wrap items-center gap-2">
							<button className={buttonClass} type="button" onClick={applyView}>
								Apply view
							</button>
							<button className={buttonClass} type="button" onClick={resetView}>
								Reset view
							</button>
						</div>
					</section>

					{showDemoBrowser && (
						<section className={sectionClass}>
							<h2 className={sectionTitleClass}>Demos</h2>
							<div className={listClass}>
								{demoEntries.length === 0 && <div className={hintClass}>Run `demo` to load entries.</div>}
								{demoEntries.map((demo) => (
									<div className={textureButtonClass} key={demo.number}>
										<div className="mb-2 text-sm font-medium">
											{demo.number}. {demo.name}
										</div>
										<div className="flex flex-wrap items-center gap-2">
											<button className={buttonClass} type="button" onClick={() => executeCommand(`demo ${demo.number}`)}>
												Run now
											</button>
											<button className={buttonClass} type="button" onClick={() => setCommandInput(`demo ${demo.number}`)}>
												Insert command
											</button>
										</div>
									</div>
								))}
							</div>
						</section>
					)}
				</aside>

				<section className={centerPanelClass}>
					<div className={editorBarClass}>
						<select
							className={`${inputClass} min-w-[11rem] flex-1`}
							value={editMode}
							onChange={(event) => setEditMode(event.target.value as "formula" | "state" | "command")}
						>
							<option value="formula">Formula Mode (=)</option>
							<option value="state">State Mode (:=)</option>
							<option value="command">Command Mode</option>
						</select>

						{editMode !== "command" && (
							<input
								className={`${inputClass} basis-60`}
								value={useBuilderTarget ? builderToken : targetTokenInput}
								onChange={(event) => {
									setUseBuilderTarget(false);
									setTargetTokenInput(event.target.value);
								}}
								placeholder="# [x,y]"
							/>
						)}

						{editMode === "command" ? (
							<input
								className={`${inputClass} min-w-[12rem] flex-1`}
								value={commandInput}
								onChange={(event) => setCommandInput(event.target.value)}
								placeholder="alloc, use, view, effects, demo..."
							/>
						) : (
							<input
								className={`${inputClass} min-w-[12rem] flex-1`}
								value={editorValue}
								onChange={(event) => setEditorValue(event.target.value)}
								placeholder="expression"
							/>
						)}

						<button className={buttonClass} type="button" onClick={runEditorApply} disabled={!ready}>
							Apply
						</button>
					</div>

					<div className={toolbarClass}>
						<span>Selection: {selectionToken || "(none)"}</span>
						<label className={checkboxLabelClass}>
							<input
								className={checkboxClass}
								type="checkbox"
								checked={showExcelLabels}
								onChange={(event) => setShowExcelLabels(event.target.checked)}
							/>
							Excel labels
						</label>
						<div className="flex flex-wrap items-end gap-2">
							<label className={`${labelClass} ${smallLabelClass}`}>
								Col
								<input
									className={inputClass}
									type="number"
									value={viewportCol}
									onChange={(event) => setViewportCol(Number(event.target.value))}
								/>
							</label>
							<label className={`${labelClass} ${smallLabelClass}`}>
								Row
								<input
									className={inputClass}
									type="number"
									value={viewportRow}
									onChange={(event) => setViewportRow(Number(event.target.value))}
								/>
							</label>
						</div>
					</div>

					{readOnlyTick && (
						<div className={readonlyBannerClass}>
							Snapshot mode: tick #{readOnlyTick.index}. Editing is disabled.
							<button className={buttonClass} type="button" onClick={() => setSelectedTickIndex(null)}>
								Return to live
							</button>
						</div>
					)}

					<div className={gridWrapClass}>
						<table className={gridTableClass}>
							<thead>
								<tr>
									<th className={tableHeaderCornerClass}>coord</th>
									{Array.from({ length: GRID_COLS }, (_, i) => {
										const col = viewportCol + i;
										const label = showExcelLabels
											? String.fromCharCode(((col % 26) + 26) % 26 + 65)
											: `#[${col}]`;
										return <th key={`h-${col}`} className={tableHeaderTopClass}>{label}</th>;
									})}
								</tr>
							</thead>
							<tbody>
								{Array.from({ length: GRID_ROWS }, (_, y) => {
									const row = viewportRow + y;
									const rowLabel = showExcelLabels ? String(row + 1) : `#[${row}]`;
									return (
										<tr key={`row-${row}`}>
											<th className={tableHeaderLeftClass}>{rowLabel}</th>
											{Array.from({ length: GRID_COLS }, (_, x) => {
												const col = viewportCol + x;
												const cell = cellMap.get(`${col}:${row}`);
												const selected =
													selectionAnchor &&
													selectionFocus &&
													col >= Math.min(selectionAnchor.col, selectionFocus.col) &&
													col <= Math.max(selectionAnchor.col, selectionFocus.col) &&
													row >= Math.min(selectionAnchor.row, selectionFocus.row) &&
													row <= Math.max(selectionAnchor.row, selectionFocus.row);
												const changed = cell ? changedSet.has(cell.tensor_token) : false;
												const displayValue = (() => {
													if (!cell) {
														return "";
													}
													if (readOnlyTick) {
														return readOnlyTick.snapshot[cell.tensor_token] ?? "";
													}
													return cell.value ?? "";
												})();

												const className = [
													gridCellBaseClass,
													selected ? "outline outline-2 -outline-offset-2 outline-cyan-300/80" : "",
													cell?.has_formula ? "bg-sky-500/15" : "",
													cell?.has_input ? "bg-emerald-500/15" : "",
													cell?.emits_effect ? "bg-amber-500/15" : "",
													changed ? "animate-pulse ring-1 ring-cyan-300/70" : "",
												]
													.filter(Boolean)
													.join(" ");

												return (
													<td
														key={`cell-${col}-${row}`}
														className={className}
														onMouseDown={(event) => {
															event.preventDefault();
															if (cell) {
																onCellMouseDown(cell, event.shiftKey);
															}
														}}
														onMouseEnter={() => {
															if (cell) {
																onCellMouseEnter(cell);
															}
														}}
													>
														{cell && (
															<>
																<div className={gridCoordClass}>{showExcelLabels ? cell.view_label : cell.tensor_token}</div>
																<div className={gridValueClass}>{displayValue}</div>
																<div className={badgesRowClass}>
																	{cell.has_formula && <span className={`${badgeBaseClass} bg-sky-300`}>f</span>}
																	{cell.has_input && <span className={`${badgeBaseClass} bg-emerald-300`}>s</span>}
																	{cell.emits_effect && <span className={`${badgeBaseClass} bg-amber-300`}>e</span>}
																</div>
															</>
														)}
													</td>
												);
											})}
										</tr>
									);
								})}
							</tbody>
						</table>
					</div>
				</section>

				<aside className={rightPanelClass}>
					<section className={sectionClass}>
						<h2 className={sectionTitleClass}>Inspector</h2>
						{!inspector && <div className={hintClass}>Select a cell to inspect.</div>}
						{inspector && (
							<div className={inspectorGridClass}>
								<div className={inspectorRowClass}>
									<span className={inspectorKeyClass}>Cell</span>
									<span>{inspector.tensor_token || "-"}</span>
								</div>
								<div className={inspectorRowClass}>
									<span className={inspectorKeyClass}>state_curr</span>
									<span>{inspector.value ?? "empty"}</span>
								</div>
								<div className={inspectorRowClass}>
									<span className={inspectorKeyClass}>formula</span>
									<span>{inspector.formula ?? "-"}</span>
								</div>
								<div className={inspectorRowClass}>
									<span className={inspectorKeyClass}>input override</span>
									<span>{inspector.input ?? "none"}</span>
								</div>
								{inspector.error && (
									<div className={inspectorRowClass}>
										<span className={inspectorKeyClass}>error</span>
										<span>{inspector.error}</span>
									</div>
								)}
							</div>
						)}
					</section>

					<section className={sectionClass}>
						<h2 className={sectionTitleClass}>Dependencies</h2>
						<div className={listClass}>
							{(inspector?.dependencies ?? []).length === 0 && (
								<div className={hintClass}>No dependencies.</div>
							)}
							{(inspector?.dependencies ?? []).map((token) => (
								<button className={dependencyButtonClass} type="button" key={`dep-${token}`} onClick={() => jumpToDependency(token)}>
									{token}
								</button>
							))}
						</div>
					</section>

					<section className={sectionClass}>
						<h2 className={sectionTitleClass}>Dependents</h2>
						<div className={listClass}>
							{(inspector?.dependents ?? []).length === 0 && (
								<div className={hintClass}>No dependents.</div>
							)}
							{(inspector?.dependents ?? []).map((token) => (
								<button className={dependencyButtonClass} type="button" key={`dpt-${token}`} onClick={() => jumpToDependency(token)}>
									{token}
								</button>
							))}
						</div>
					</section>

					<section className={sectionClass}>
						<h2 className={sectionTitleClass}>Slice Builder</h2>
						<div className={sliceBuilderClass}>
							<div className={axisBlockClass}>
								<span className={axisLabelClass}>X axis</span>
								<select className={inputClass} value={xMode} onChange={(event) => setXMode(event.target.value as AxisSliceMode)}>
									<option value="single">single</option>
									<option value="span">a:b</option>
									<option value="full">:</option>
									<option value="prefix">:n</option>
									<option value="suffix">n:</option>
								</select>
								<input className={inputClass} type="number" value={xStart} onChange={(event) => setXStart(Number(event.target.value))} />
								<input className={inputClass} type="number" value={xEnd} onChange={(event) => setXEnd(Number(event.target.value))} />
							</div>
							<div className={axisBlockClass}>
								<span className={axisLabelClass}>Y axis</span>
								<select className={inputClass} value={yMode} onChange={(event) => setYMode(event.target.value as AxisSliceMode)}>
									<option value="single">single</option>
									<option value="span">a:b</option>
									<option value="full">:</option>
									<option value="prefix">:n</option>
									<option value="suffix">n:</option>
								</select>
								<input className={inputClass} type="number" value={yStart} onChange={(event) => setYStart(Number(event.target.value))} />
								<input className={inputClass} type="number" value={yEnd} onChange={(event) => setYEnd(Number(event.target.value))} />
							</div>
							<label className={checkboxLabelClass}>
								<input
									className={checkboxClass}
									type="checkbox"
									checked={useBuilderTarget}
									onChange={(event) => setUseBuilderTarget(event.target.checked)}
								/>
								Use builder as edit target
							</label>
							<div className={builderTokenClass}>{builderToken}</div>
						</div>
					</section>

					<section className={`${sectionClass} lg:col-span-2 2xl:col-span-1`}>
						<h2 className={sectionTitleClass}>Command Parity</h2>
						<div className={hintClass}>Current command: {editMode === "command" ? commandInput : `${effectiveTargetToken} ${editMode === "formula" ? "=" : ":="} ${editorValue}`}</div>
						<div className="flex flex-wrap items-center gap-2">
							<button className={buttonClass} type="button" onClick={copyAsScript}>
								Copy as script
							</button>
							<button className={buttonClass} type="button" onClick={openTerminalMode}>
								Open in terminal mode
							</button>
						</div>
					</section>
				</aside>
			</main>

			<footer className={bottomClass}>
				<section className={bottomPaneClass}>
					<h3 className={sectionTitleClass}>Console</h3>
					<div className={consolePaneClass}>
						{logs.map((entry) => (
							<div key={entry.id} className="flex gap-2 text-xs">
								<span className="shrink-0 text-slate-400">{toTimeLabel(entry.at)}</span>
								<span
									className={`break-words ${
										entry.kind === "cmd"
											? "text-cyan-200"
											: entry.kind === "error"
												? "text-rose-300"
												: entry.kind === "info"
													? "text-amber-300"
													: "text-slate-200"
									}`}
								>
									{entry.text}
								</span>
							</div>
						))}
					</div>
				</section>

				<section className={bottomPaneClass}>
					<h3 className={sectionTitleClass}>Tick Timeline</h3>
					<div className={timelineControlsClass}>
						<button className={buttonClass} type="button" onClick={() => setSelectedTickIndex(null)}>
							Live
						</button>
						<button className={buttonClass} type="button" onClick={clearTimeline}>
							Clear
						</button>
					</div>
					<div className={timelineListClass}>
						{timeline.map((entry) => (
							<button
								type="button"
								key={`tick-${entry.index}`}
								className={`${textureButtonClass} text-xs ${
									selectedTickIndex === entry.index
										? "border-cyan-400/60 bg-cyan-500/10"
										: ""
								}`}
								onClick={() => setSelectedTickIndex(entry.index)}
							>
								<div className="font-semibold">#{entry.index} {entry.command}</div>
								<div className="text-slate-300">
									changed {entry.changed_cells} | queued {entry.enqueued_effects} | pending {entry.pending_effects}
								</div>
								<div className="text-slate-400">{entry.duration_ms.toFixed(2)} ms</div>
							</button>
						))}
					</div>
				</section>

				<section className={bottomPaneClass}>
					<h3 className={sectionTitleClass}>Effects</h3>
					<div className="flex flex-wrap items-center gap-2">
						<button className={buttonClass} type="button" onClick={() => executeCommand("effects pending")}>pending</button>
						<button className={buttonClass} type="button" onClick={() => executeCommand("effects run 1")}>Run 1</button>
						<button className={buttonClass} type="button" onClick={() => executeCommand("effects run 10")}>Run 10</button>
						<button className={buttonClass} type="button" onClick={() => executeCommand("effects run")}>Run all</button>
					</div>
					<div className={effectsListClass}>
						{effectRuns.map((entry) => (
							<div key={entry.id} className={`${textureButtonClass} text-xs`}>
								<div className="font-semibold">{toTimeLabel(entry.at)} {entry.command}</div>
								<div className="text-slate-300">{entry.summary}</div>
								<div className="text-slate-400">pending after: {entry.pending_after}</div>
							</div>
						))}
					</div>
				</section>
			</footer>
		</div>
	);
}

export default StudioApp;
