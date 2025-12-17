import "./App.css";
import { Sheet } from "../../pkg";
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

		const sheet = new Sheet();
		sheetRef.current = sheet;

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

		const refElement = terminalRef.current;
		if (refElement) {
			refElement.addEventListener("keydown", handleKeydownCapture, {
				capture: true,
			});

			refElement.addEventListener("paste", handlePaste, {
				capture: true,
			});
		}

		init().then(() => {
			tmp_term = new Terminal({
				fontSize: 14,
				scrollback: 10000,
				cursorBlink: true,
			});

			const fitAddon = new FitAddon();
			tmp_term.loadAddon(fitAddon);
			tmp_term.open(terminalRef.current!);

			tmp_term.writeln("SheetLang Terminal");
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
			// Handle 'tick' command
			if (trimmed === "tick") {
				sheet.tick();
				terminal.writeln("Tick processed.");
				return;
			}

			// Handle 'show' command
			if (trimmed === "show") {
				terminal.writeln("Current State:");
				const values = sheet.get_all_values();
				if (values.trim()) {
					// Split by lines and write each one separately
					const lines = values.split("\n").filter((line) => line.trim());
					lines.forEach((line) => terminal.writeln(line));
				} else {
					terminal.writeln("(empty)");
				}
				return;
			}

			// Handle 'help' command
			if (trimmed === "help") {
				terminal.writeln("SheetLang Commands:");
				terminal.writeln("  A1 = <formula>  - Set a formula for a cell");
				terminal.writeln("  tick            - Advance the engine one step");
				terminal.writeln("  show            - Display all cell values");
				terminal.writeln("  help            - Show this help message");
				terminal.writeln("  exit            - (not implemented in web)");
				terminal.writeln("");
				terminal.writeln("Examples:");
				terminal.writeln("  A1 = 10");
				terminal.writeln("  B1 = A1 + 5");
				terminal.writeln("  tick");
				terminal.writeln("  show");
				return;
			}

			// Try to parse as assignment (A1 = ...)
			const assignMatch = trimmed.match(/^([A-Z]\d+)\s*=\s*(.+)$/);
			if (assignMatch) {
				const [, cell, formula] = assignMatch;
				sheet.set_formula(cell, formula);
				terminal.writeln(`Formula set for ${cell}`);
				return;
			}

			// If we get here, unknown command
			terminal.writeln(`Unknown command: ${trimmed}`);
			terminal.writeln(`Type 'help' for available commands`);
		} catch (e) {
			terminal.writeln(`Error: ${e}`);
		}
	};

	return (
		<div
			ref={terminalRef}
			style={{ height: "100%", width: "100%", textAlign: "left" }}
		></div>
	);
}

export default App;
