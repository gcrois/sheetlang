import "./App.css";
import init_wasm, { Sheet } from "../pkg/sheetlang";
import { useEffect, useRef, useState } from "react";
import { FitAddon, init, Terminal } from "ghostty-web";

function App() {
	const [_term, setTerm] = useState<Terminal | null>(null);
	const terminalRef = useRef<HTMLDivElement>(null);
	const sheetRef = useRef<Sheet | null>(null);
	const historyRef = useRef<string[]>([]);
	const historyIndexRef = useRef<number>(-1);

	useEffect(() => {
		let tmp_term: Terminal | null = null;
		let resizeObserver: ResizeObserver | null = null;
		let currentLine = "";
		const promptStr = "$ ";

		const handleInput = (data: string) => {
			if (!tmp_term) return;
			const ord = data.charCodeAt(0);

			if (data === "\x1b[A") {
				// Up Arrow
				const history = historyRef.current;
				let idx = historyIndexRef.current;
				if (history.length > 0) {
					idx = Math.min(idx + 1, history.length - 1);
					historyIndexRef.current = idx;
					const cmd = history[history.length - 1 - idx];
					tmp_term.write("\r" + promptStr + "\x1b[K" + cmd);
					currentLine = cmd;
				}
				return;
			} else if (data === "\x1b[B") {
				// Down Arrow
				const history = historyRef.current;
				let idx = historyIndexRef.current;
				if (idx > -1) {
					idx -= 1;
					historyIndexRef.current = idx;
					if (idx === -1) {
						currentLine = "";
						tmp_term.write("\r" + promptStr + "\x1b[K");
					} else {
						const cmd = history[history.length - 1 - idx];
						currentLine = cmd;
						tmp_term.write("\r" + promptStr + "\x1b[K" + cmd);
					}
				}
				return;
			}

			if (ord === 13) {
				// Enter
				tmp_term.write("\r\n");
				if (currentLine.trim()) historyRef.current.push(currentLine);
				historyIndexRef.current = -1;
				processCommand(currentLine, tmp_term, sheetRef.current);
				currentLine = "";
				tmp_term.write(promptStr);
			} else if (ord === 127) {
				// Backspace
				if (currentLine.length > 0) {
					currentLine = currentLine.slice(0, -1);
					tmp_term.write("\b \b");
				}
			} else if (ord === 3) {
				// Ctrl+C
				tmp_term.write("^C\r\n");
				currentLine = "";
				historyIndexRef.current = -1;
				tmp_term.write(promptStr);
			} else if (ord === 21) {
				// Ctrl+U
				tmp_term.write("\r" + promptStr + "\x1b[K");
				currentLine = "";
			} else if (ord >= 32) {
				currentLine += data;
				tmp_term.write(data);
			}
		};

		const handleKeydownCapture = (e: KeyboardEvent) => {
			const isModifier = e.metaKey || e.ctrlKey;

			if (isModifier) {
				const key = e.key.toLowerCase();

				const browserShortcuts = [
					// Tab/Window management
					"w",
					"t",
					"r",
					"n",
					"f",
					// Number switching
					"0",
					"1",
					"2",
					"3",
					"4",
					"5",
					"6",
					"7",
					"8",
					"9",
					// Zooming: - (minus), = (equals/plus), + (numpad plus or shift+=)
					"-",
					"=",
					"+",
				];

				if (browserShortcuts.includes(key)) {
					e.stopPropagation();
				}
			}
		};

		const handlePaste = (e: ClipboardEvent) => {
			e.preventDefault();
			e.stopPropagation();

			const text = e.clipboardData?.getData("text");
			if (text && tmp_term) {
				// Split by newlines and process each line
				const lines = text.split(/\r?\n/);

				for (let i = 0; i < lines.length; i++) {
					const line = lines[i].trim();
					if (line) {
						// Write the line to the terminal
						tmp_term.write(line);
						currentLine = line;

						// Simulate Enter press to execute the command
						tmp_term.write("\r\n");
						historyRef.current.push(line);
						historyIndexRef.current = -1;
						processCommand(line, tmp_term, sheetRef.current);
						currentLine = "";
						tmp_term.write(promptStr);
					}
				}
			}
		};

		const handleDragOver = (e: DragEvent) => {
			e.preventDefault();
			e.stopPropagation();
		};

		const handleDrop = (e: DragEvent) => {
			e.preventDefault();
			e.stopPropagation();

			const files = e.dataTransfer?.files;
			if (files && files.length > 0 && tmp_term) {
				const file = files[0];

				// Check if it's a .sheet file
				if (!file.name.endsWith('.sheet')) {
					tmp_term.writeln(`\r\nError: Only .sheet files are supported (got: ${file.name})`);
					tmp_term.write(promptStr);
					return;
				}

				const reader = new FileReader();
				reader.onload = (event) => {
					const text = event.target?.result as string;
					if (text && tmp_term) {
						tmp_term.writeln(`\r\nLoading ${file.name}...\r\n`);

						// Split by newlines and process each line
						const lines = text.split(/\r?\n/);
						let executedCount = 0;

						for (let i = 0; i < lines.length; i++) {
							const line = lines[i].trim();
							// Skip empty lines
							if (line) {
								// Write the line to the terminal
								tmp_term.write(promptStr + line + "\r\n");

								// Add to history
								historyRef.current.push(line);
								historyIndexRef.current = -1;

								// Execute the command
								processCommand(line, tmp_term, sheetRef.current);
								executedCount++;
							}
						}

						tmp_term.writeln(`\r\nExecuted ${executedCount} command(s) from ${file.name}\r\n`);
						currentLine = "";
						tmp_term.write(promptStr);
					}
				};
				reader.readAsText(file);
			}
		};

		const refElement = terminalRef.current;
		if (refElement) {
			refElement.addEventListener("keydown", handleKeydownCapture, {
				capture: true,
			});

			refElement.addEventListener("paste", handlePaste, {
				capture: true,
			});

			refElement.addEventListener("dragover", handleDragOver);
			refElement.addEventListener("drop", handleDrop);
		}

		// Initialize WASM first, then terminal
		init_wasm().then(() => {
			const sheet = new Sheet();
			sheetRef.current = sheet;

			return init();
		}).then(() => {
			tmp_term = new Terminal({
				fontSize: 14,
				scrollback: 10000,
				cursorBlink: true,
			});

			const fitAddon = new FitAddon();
			tmp_term.loadAddon(fitAddon);
			tmp_term.open(terminalRef.current!);

			tmp_term.writeln("SheetLang Terminal");
			const buildInfo = (sheetRef.current as { build_info?: () => string } | null)?.build_info?.();
			if (buildInfo) {
				tmp_term.writeln(buildInfo);
			}
			tmp_term.writeln("A reactive spreadsheet programming language");
			tmp_term.writeln("");
			tmp_term.writeln("Type 'help' for available commands");
			tmp_term.write("\r\n" + promptStr);

			if (terminalRef.current) {
				resizeObserver = new ResizeObserver(() => {
					try {
						fitAddon.fit();
					} catch (e) {
						console.error("Error fitting terminal:", e);
					}
				});
				resizeObserver.observe(terminalRef.current);
			}

			tmp_term.onData(handleInput);

			setTerm(tmp_term);
			fitAddon.fit();
			tmp_term.focus();

			// Parse and execute code from query parameters
			const urlParams = new URLSearchParams(window.location.search);
			const codeParam = urlParams.get("code");

			if (codeParam && tmp_term && sheetRef.current) {
				// Split by newlines to get individual commands
				const lines = codeParam.split(/\r?\n/).map(line => line.trim()).filter(line => line);

				if (lines.length > 0) {
					tmp_term.writeln("Executing code from URL...");
					tmp_term.writeln("");

					for (const line of lines) {
						// Display the command
						tmp_term.write(promptStr + line + "\r\n");

						// Add to history
						historyRef.current.push(line);

						// Execute the command
						processCommand(line, tmp_term, sheetRef.current);
					}

					tmp_term.writeln("");
					tmp_term.write(promptStr);
				}
			}
		});

		return () => {
			if (refElement) {
				refElement.removeEventListener(
					"keydown",
					handleKeydownCapture,
					{ capture: true }
				);
				refElement.removeEventListener("paste", handlePaste, {
					capture: true,
				});
				refElement.removeEventListener("dragover", handleDragOver);
				refElement.removeEventListener("drop", handleDrop);
			}
			tmp_term?.dispose();
		};
	}, []);

	const processCommand = (
		input: string,
		terminal: Terminal,
		sheet: Sheet | null
	) => {
		if (!input.trim()) return;
		if (!sheet) {
			terminal.writeln("Error: Sheet not initialized");
			return;
		}

		const trimmed = input.trim();

		try {
			// Special handling for 'encode' - needs access to history
			if (trimmed === "encode") {
				const history = historyRef.current;
				if (history.length === 0) {
					terminal.writeln("No command history to encode");
					return;
				}

				// Join history with newlines and encode
				const code = history.join("\n");
				const encoded = encodeURIComponent(code);
				const url = `${window.location.origin}${window.location.pathname}?code=${encoded}`;

				// Update URL bar without reload
				window.history.pushState({}, "", url);

				// Copy to clipboard
				navigator.clipboard.writeText(url).then(() => {
					terminal.writeln("URL updated and copied to clipboard!");
					terminal.writeln(`Commands encoded: ${history.length}`);
				}).catch(() => {
					terminal.writeln("URL updated! (Clipboard copy failed - copy from address bar)");
					terminal.writeln(`Commands encoded: ${history.length}`);
				});
				return;
			}

			// Use unified WASM command execution for everything else
			const result = sheet.execute_command?.(trimmed);
			if (!result) {
				terminal.writeln("Error: execute_command not available (rebuild WASM)");
				return;
			}

			// Handle result based on type
			if (result.type === "output") {
				const lines = result.text.split("\n");
				lines.forEach((line: string) => {
					if (line) terminal.writeln(line);
				});
			} else if (result.type === "bshow") {
				// Render binary visualization
				renderBShow(result.coords, sheet, terminal);
			} else if (result.type === "demo") {
				// Execute demo script line by line (same logic as drag-and-drop)
				terminal.writeln("Loading demo...\r\n");
				const lines = result.script.split(/\r?\n/);

				for (const line of lines) {
					const trimmed = line.trim();
					if (!trimmed) {
						continue;
					}

					// Display and execute the command
					terminal.write("$ " + trimmed + "\r\n");
					historyRef.current.push(trimmed);
					processCommand(trimmed, terminal, sheet);
				}

				terminal.writeln("\r\nDemo completed\r\n");
			} else if (result.type === "error") {
				console.log("Error result:", result);

				// Highlight the error position if span info is available
				if (result.span_start !== undefined && result.span_end !== undefined) {
					// Account for the prompt "$ " (2 characters) when positioning the indicator
					const promptLength = 2;
					const spanLength = Math.max(1, result.span_end - result.span_start);
					const indicator = " ".repeat(promptLength + result.span_start) + "\x1b[31m" + "^".repeat(spanLength) + "\x1b[0m";
					terminal.writeln(indicator);
				}

				terminal.writeln(`Error: ${result.message}`);
			} else if (result.type === "exit") {
				terminal.writeln("Exit command is only available in the REPL");
			}
		} catch (e) {
			terminal.writeln(`Error: ${e}`);
		}
	};

	const renderBShow = (coords: string[], sheet: Sheet, terminal: Terminal) => {
		if (!coords || coords.length === 0) {
			terminal.writeln("(empty)");
			return;
		}

		// Parse coordinates and find bounds
		const parsedCoords = coords.map(c => {
			const match = c.match(/^([A-Z])(\d+)$/);
			if (!match) return null;
			return {
				col: match[1].charCodeAt(0) - 65,
				row: parseInt(match[2])
			};
		}).filter(c => c !== null);

		if (parsedCoords.length === 0) {
			terminal.writeln("(empty)");
			return;
		}

		const startColNum = Math.min(...parsedCoords.map(c => c!.col));
		const endColNum = Math.max(...parsedCoords.map(c => c!.col));
		const startRowNum = Math.min(...parsedCoords.map(c => c!.row));
		const endRowNum = Math.max(...parsedCoords.map(c => c!.row));

		const cellWidth = 2;

		// Read values from the grid
		const grid: boolean[][] = [];
		for (let row = startRowNum; row <= endRowNum; row++) {
			const gridRow: boolean[] = [];
			for (let col = startColNum; col <= endColNum; col++) {
				const cell = String.fromCharCode(65 + col) + row;
				const value = sheet.get_value(cell);
				// Truthy: not "empty", not "0", not empty string
				const isTruthy = value !== "empty" && value !== "0" && value.trim() !== "";
				gridRow.push(isTruthy);
			}
			grid.push(gridRow);
		}

		// Display the grid
		const fullBlock = "█";
		const emptyBlock = " ";

		// Top border
		terminal.writeln("┌" + "─".repeat((endColNum - startColNum + 1) * cellWidth) + "┐");

		// Grid rows
		for (const row of grid) {
			const rowStr = row.map(isTruthy =>
				(isTruthy ? fullBlock : emptyBlock).repeat(cellWidth)
			).join("");
			terminal.writeln("│" + rowStr + "│");
		}

		// Bottom border
		terminal.writeln("└" + "─".repeat((endColNum - startColNum + 1) * cellWidth) + "┘");
	};

	return (
		<div
			ref={terminalRef}
			style={{ height: "100%", width: "100%", textAlign: "left" }}
		></div>
	);
}

export default App;
