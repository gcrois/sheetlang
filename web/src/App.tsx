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
		sheet.set_formula("A1", "10");
		sheetRef.current = sheet;

		let currentLine = "";
		const promptStr = "$ ";

		const handleInput = (data: string) => {
			if (!tmp_term) return;
			const ord = data.charCodeAt(0);

			if (data === "\x1b[A") {
				// Up Arrow logic...
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
				// Down Arrow logic...
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

		// --- 2. Event Listeners ---

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
			if (text) {
				const cleanText = text.replace(/[\r\n]+/g, " ");
				handleInput(cleanText);
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
			tmp_term.writeln("https://g.regory.dev/blog/langjam-gamejam/day_-1/");
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
		try {
			terminal.writeln(`Executing: ${input}`);
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
