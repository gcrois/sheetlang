# SheetLang

A reactive spreadsheet programming language with a temporal computation model.

## Overview

SheetLang is a programming language that combines the familiarity of spreadsheet formulas with the power of functional programming. It features a reactive engine that updates cell values based on their formulas, supporting temporal computation where cells can reference their previous state.

## Features

- **Spreadsheet-style cells**: Reference cells like `A1`, `B2`, etc.
- **Relative references**: Use `$(-1, 0)` to reference cells relative to the current cell
- **Reactive engine**: Cells automatically update when dependencies change via `tick` operations
- **Rich type system**: Integers, booleans, strings, arrays, dictionaries, ranges, and lambdas
- **Functional programming**: First-class functions with lambda expressions
- **Temporal computation**: Access previous state of cells across ticks
- **WASM support**: Run in the browser via WebAssembly

## Installation

### Prerequisites

- Rust (latest stable version)
- wasm-pack (for building the web version)

### Building

```bash
# Build the native CLI
cargo build --release

# Run tests
cargo test
```

## Usage

### Native CLI

Run the interactive REPL:

```bash
cargo run
```

Example session:

```
SheetLang
Commands: 'tick', 'show', or assignments 'A1 = ...'
>> A1 = 10
Formula set for A1
>> B1 = A1 + 5
Formula set for B1
>> tick
Tick processed.
>> show
Current State:
A1: 10
B1: 15
>> exit
```

### Web Terminal

See [web/README.md](web/README.md) for running the web-based terminal.

## Language Reference

### Basic Types

```javascript
10          // Integer
true        // Boolean
"hello"     // String
[1, 2, 3]   // Array
{x: 10}     // Dictionary
1..10       // Range
```

### Cell References

```javascript
A1 = 10               // Absolute reference
B1 = A1 + 5           // Reference another cell
C1 = $(-1, 0)         // Relative reference (cell to the left)
```

### Operators

```javascript
+  -  *  /            // Arithmetic
== != > <             // Comparison
and or not            // Logical
```

### Control Flow

```javascript
A1 = if A2 > 10 then "high" else "low"
```

### Functions (Lambdas)

```javascript
A1 = \x -> x * 2              // Lambda definition
B1 = A1(5)                    // Function call, returns 10
```

### Data Structures

```javascript
// Arrays
A1 = [1, 2, 3]
B1 = A1[0]               // Index access, returns 1

// Dictionaries
A1 = {name: "Alice", age: 30}
B1 = A1.name             // Member access, returns "Alice"
C1 = A1["age"]           // Index access, returns 30
```

### Temporal Computation

The `tick` command advances the engine by one step, updating all cells based on their formulas. Cells can reference their previous state indirectly through other cells.

```javascript
A1 = 0
A2 = A1 + 1
tick        // A2 becomes 1
tick        // A2 becomes 2 (because A1 was 1 from previous tick)
```

## Testing

Run the test suite:

```bash
cargo test
```

Snapshot tests are located in [tests/](tests/) using `insta`.

