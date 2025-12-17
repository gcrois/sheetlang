# SheetLang Web Terminal

A web-based terminal interface for SheetLang, running via WebAssembly.

See it live here: https://sheetlang.pages.dev

## Overview

This is a browser-based REPL for SheetLang that provides the same functionality as the native CLI, powered by WebAssembly. It features a terminal interface built with [ghostty-web](https://github.com/mitchellh/ghostty) and React.

## Features

- Full SheetLang interpreter running in the browser
- Terminal-style interface with command history
- Keyboard shortcuts (arrow keys for history, Ctrl+C, Ctrl+U)
- No backend required - runs entirely client-side
- Fast and responsive thanks to WASM

## Development

### Prerequisites

- Node.js (v18+)
- pnpm
- Rust & wasm-pack (for building the WASM module)

### Setup

1. Build the WASM module from the project root:

```bash
wasm-pack build --target web --out-dir web/pkg
```

2. Install dependencies:

```bash
cd web
pnpm install
```

3. Start the development server:

```bash
pnpm dev
```

4. Open your browser to the URL shown (usually http://localhost:5173)

### Building for Production

```bash
# Build WASM (from project root)
wasm-pack build --target web --out-dir web/pkg

# Build web app
cd web
pnpm build

# Preview production build
pnpm preview
```

## Usage

Once the terminal loads, you can use the following commands:

### Commands

- `A1 = <formula>` - Set a formula for a cell
- `tick` - Advance the engine one step
- `show` - Display all cell values
- `help` - Show available commands

### Example Session

```
$ A1 = 10
Formula set for A1
$ B1 = A1 * 2
Formula set for B1
$ tick
Tick processed.
$ show
Current State:
A1: 10
B1: 20
```

### WASM Integration

The SheetLang interpreter is compiled to WebAssembly and exposed via the `Sheet` class:

```typescript
import { Sheet } from "../../pkg";

const sheet = new Sheet();
sheet.set_formula("A1", "10");
sheet.tick();
const values = sheet.get_all_values();
```
