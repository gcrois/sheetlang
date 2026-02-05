//! Collaborative editing support using Loro CRDT
//!
//! **Architecture**: Deterministic Simulation Engine driven by a Sparse Timeline of Inputs
//!
//! Instead of syncing computed grid state, we sync the **timeline of user edits** (inputs at specific ticks).
//! Each client replays these inputs through the Engine's tick logic to compute the final grid state deterministically.
//!
//! ## Loro Schema
//! ```
//! timeline (LoroMap)
//!   ├─ "0" (LoroMap) → edits made at tick 0
//!   │   ├─ "0,1" → LoroValue::String("=A1+1")  (formula)
//!   │   └─ "2,3" → LoroValue::I64(42)          (number)
//!   ├─ "5" (LoroMap) → edits made at tick 5
//!   │   └─ "1,1" → LoroValue::String("hello")
//!   └─ ...
//! metadata (LoroMap)
//!   └─ "current_tick" → LoroValue::I64(5)
//! ```
//!
//! ## Key Benefits
//! - **Sparse storage**: Only store ticks where users actually edited
//! - **Deterministic**: Same inputs → same output on all clients
//! - **Native types**: Use LoroValue::I64 for numbers, String for formulas
//! - **Automatic conflict resolution**: Loro handles concurrent edits via CRDT
//! - **Immutable history**: Past ticks don't change (can cache/snapshot)

use crate::ast::{Expr, Value};
use crate::interpreter::{Coord, Engine};
use crate::parser::parser;
use crate::lexer::Token;
use chumsky::Parser;
use logos::Logos;
use loro::{ExportMode, LoroDoc, LoroValue};

// --- Cell Content Types ---

/// Represents the content of a single cell in the timeline
#[derive(Debug, Clone, PartialEq)]
pub enum CellContent {
    Empty,
    Number(i64),      // Native Loro I64 - matches SheetLang's Value::Int
    Formula(String),  // Native Loro String - full formula text
}

impl CellContent {
    /// Convert from Loro's native LoroValue to strongly-typed CellContent
    pub fn from_loro_value(v: LoroValue) -> Self {
        match v {
            LoroValue::Null => CellContent::Empty,
            LoroValue::I64(n) => CellContent::Number(n),
            LoroValue::String(s) => CellContent::Formula(s.to_string()),
            _ => CellContent::Empty, // Fallback for unexpected types
        }
    }

    /// Convert to Loro's native LoroValue for storage
    pub fn to_loro_value(&self) -> LoroValue {
        match self {
            CellContent::Empty => LoroValue::Null,
            CellContent::Number(n) => LoroValue::I64(*n),
            CellContent::Formula(s) => LoroValue::String(s.clone().into()),
        }
    }

    /// Convert from SheetLang's Value type (for convenience)
    pub fn from_value(v: &Value) -> Self {
        match v {
            Value::Empty => CellContent::Empty,
            Value::Int(n) => CellContent::Number(*n),
            _ => CellContent::Empty, // Complex types not directly supported in timeline
        }
    }

    /// Parse CellContent into an Expr for the Engine
    pub fn to_expr(&self) -> Result<Expr, String> {
        match self {
            CellContent::Empty => Ok(Expr::Literal(Value::Empty)),
            CellContent::Number(n) => Ok(Expr::Literal(Value::Int(*n))),
            CellContent::Formula(s) => parse_expr(s),
        }
    }
}

// --- Collaboration Session ---

/// Manages the collaborative timeline of edits using Loro CRDT
pub struct CollaborationSession {
    doc: LoroDoc,
    current_tick: u64,
}

impl CollaborationSession {
    /// Create a new collaboration session
    pub fn new() -> Self {
        let doc = LoroDoc::new();

        // Initialize containers
        doc.get_map("timeline");
        let metadata = doc.get_map("metadata");
        metadata.insert("current_tick", LoroValue::I64(0)).unwrap();

        Self {
            doc,
            current_tick: 0,
        }
    }

    /// Record a user edit at the current tick
    pub fn set_cell(&mut self, coord: Coord, content: CellContent) {
        // Build a hierarchical key: "timeline/{tick}/{row},{col}"
        let key = format!("timeline/{}/{},{}", self.current_tick, coord.row, coord.col);

        // Store directly in a flat map with hierarchical keys
        // This is simpler than nested containers
        let data = self.doc.get_map("data");
        data.insert(&key, content.to_loro_value()).unwrap();

        // Update metadata
        let metadata = self.doc.get_map("metadata");
        metadata.insert("current_tick", LoroValue::I64(self.current_tick as i64)).unwrap();
    }

    /// Get all edits made at a specific tick
    pub fn get_inputs_at_tick(&self, tick: u64) -> Vec<(Coord, CellContent)> {
        let data = self.doc.get_map("data");
        let prefix = format!("timeline/{}/", tick);
        let mut inputs = Vec::new();

        // Iterate through all keys in the data map
        for key in data.keys() {
            // Check if this key belongs to the requested tick
            if key.starts_with(&prefix) {
                // Extract "row,col" part after the prefix
                if let Some(coord_str) = key.strip_prefix(&prefix) {
                    // Parse coordinate from "row,col" format
                    let parts: Vec<&str> = coord_str.split(',').collect();
                    if parts.len() == 2 {
                        if let (Ok(row), Ok(col)) = (parts[0].parse(), parts[1].parse()) {
                            // Get the value for this key
                            if let Some(value_container) = data.get(&key) {
                                if let Ok(loro_value) = value_container.into_value() {
                                    let coord = Coord { row, col };
                                    let content = CellContent::from_loro_value(loro_value);
                                    inputs.push((coord, content));
                                }
                            }
                        }
                    }
                }
            }
        }

        inputs
    }

    /// Advance to the next tick
    pub fn tick(&mut self) {
        self.current_tick += 1;

        // Update metadata in Loro
        let metadata = self.doc.get_map("metadata");
        metadata.insert("current_tick", LoroValue::I64(self.current_tick as i64)).unwrap();
    }

    /// Get the current tick number
    pub fn current_tick(&self) -> u64 {
        self.current_tick
    }

    /// Set the current tick (used when importing)
    pub fn set_current_tick(&mut self, tick: u64) {
        self.current_tick = tick;
    }

    /// Export updates for network synchronization
    pub fn export_updates(&self) -> Result<Vec<u8>, String> {
        self.doc.export(ExportMode::Snapshot)
            .map_err(|e| format!("Export failed: {:?}", e))
    }

    /// Import updates from another client
    pub fn import_updates(&mut self, data: &[u8]) -> Result<(), String> {
        self.doc.import(data)
            .map_err(|e| format!("Import failed: {:?}", e))?;

        // Sync current_tick from Loro metadata
        let metadata = self.doc.get_map("metadata");
        if let Some(tick_container) = metadata.get("current_tick") {
            if let Ok(loro_value) = tick_container.into_value() {
                if let Some(tick) = loro_value.as_i64() {
                    self.current_tick = *tick as u64;
                }
            }
        }

        Ok(())
    }

    /// Get the peer ID of this session
    pub fn peer_id(&self) -> u64 {
        self.doc.peer_id()
    }

    /// Set the peer ID of this session
    pub fn set_peer_id(&mut self, peer_id: u64) -> Result<(), String> {
        self.doc.set_peer_id(peer_id)
            .map_err(|e| format!("Failed to set peer ID: {:?}", e))
    }
}

impl Default for CollaborationSession {
    fn default() -> Self {
        Self::new()
    }
}

// --- Engine Integration ---

/// Compute the grid state at a specific tick by replaying the timeline
///
/// This is the core replay logic:
/// 1. Clear engine state
/// 2. For each tick from 0 to target_tick:
///    a. Apply user edits from the timeline
///    b. Run Engine::tick() to compute values
/// 3. Final state is in engine.state_curr
///
/// TODO (with Salsa): Replace full replay with incremental computation
/// Salsa will track dependencies and only recompute affected cells
pub fn compute_grid_at_tick(
    session: &CollaborationSession,
    engine: &mut Engine,
    target_tick: u64,
) -> Result<(), String> {
    // Clear engine state for fresh replay
    engine.formulas.clear();
    engine.state_curr.clear();
    engine.state_prev.clear();

    // Replay from tick 0 to target_tick
    for t in 0..=target_tick {
        // Step 1: Apply user edits from this tick's timeline
        let edits = session.get_inputs_at_tick(t);
        for (coord, content) in edits {
            match content {
                CellContent::Number(n) => {
                    // Set as constant literal formula
                    let expr = Expr::Literal(Value::Int(n));
                    engine.formulas.insert(coord, expr);
                }
                CellContent::Formula(s) => {
                    // Parse and set formula
                    let expr = parse_expr(&s)?;
                    engine.formulas.insert(coord, expr);
                }
                CellContent::Empty => {
                    // Remove formula (cell becomes empty/0)
                    engine.formulas.remove(&coord);
                }
            }
        }

        // Step 2: Run tick to compute values
        // Engine::tick() does double-buffering:
        //   - state_prev = state_curr
        //   - Evaluate all formulas against state_prev
        //   - Store results in state_curr
        engine.tick();
    }

    Ok(())
}

// --- Helper Functions ---

/// Parse a formula string into an Expr using the standard parsing pipeline
fn parse_expr(formula: &str) -> Result<Expr, String> {
    let token_iter = Token::lexer(formula)
        .spanned()
        .map(|(tok, _span)| tok.unwrap_or(Token::Dot));

    let token_stream = chumsky::input::Stream::from_iter(token_iter);

    parser()
        .parse(token_stream)
        .into_result()
        .map_err(|errs| format!("Parse error: {:?}", errs))
}

// --- Tests ---

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cell_content_conversions() {
        // Number conversion
        let num = CellContent::Number(42);
        let loro_val = num.to_loro_value();
        assert_eq!(CellContent::from_loro_value(loro_val), num);

        // Formula conversion
        let formula = CellContent::Formula("=A1+1".to_string());
        let loro_val = formula.to_loro_value();
        assert_eq!(CellContent::from_loro_value(loro_val), formula);

        // Empty conversion
        let empty = CellContent::Empty;
        let loro_val = empty.to_loro_value();
        assert_eq!(CellContent::from_loro_value(loro_val), empty);
    }

    #[test]
    fn test_session_basic() {
        let mut session = CollaborationSession::new();
        assert_eq!(session.current_tick(), 0);

        // Set a cell at tick 0
        let coord = Coord { row: 0, col: 0 }; // A1
        session.set_cell(coord, CellContent::Number(42));

        // Verify we can read it back
        let inputs = session.get_inputs_at_tick(0);
        assert_eq!(inputs.len(), 1);
        assert_eq!(inputs[0].0, coord);
        assert_eq!(inputs[0].1, CellContent::Number(42));
    }

    #[test]
    fn test_session_tick_advance() {
        let mut session = CollaborationSession::new();

        // Edit at tick 0
        session.set_cell(Coord { row: 0, col: 0 }, CellContent::Number(10));

        // Advance to tick 1
        session.tick();
        assert_eq!(session.current_tick(), 1);

        // Edit at tick 1
        session.set_cell(Coord { row: 0, col: 1 }, CellContent::Number(20));

        // Verify both ticks have data
        assert_eq!(session.get_inputs_at_tick(0).len(), 1);
        assert_eq!(session.get_inputs_at_tick(1).len(), 1);

        // Verify tick 2 is empty
        assert_eq!(session.get_inputs_at_tick(2).len(), 0);
    }

    #[test]
    fn test_export_import() {
        let mut session1 = CollaborationSession::new();
        session1.set_peer_id(1).unwrap();
        session1.set_cell(Coord { row: 0, col: 0 }, CellContent::Number(42));

        // Export from session1
        let data = session1.export_updates().unwrap();

        // Import into session2
        let mut session2 = CollaborationSession::new();
        session2.set_peer_id(2).unwrap();
        session2.import_updates(&data).unwrap();

        // Verify session2 has the edit
        let inputs = session2.get_inputs_at_tick(0);
        assert_eq!(inputs.len(), 1);
        assert_eq!(inputs[0].1, CellContent::Number(42));
    }

    #[test]
    fn test_compute_grid_simple() {
        let mut session = CollaborationSession::new();
        let mut engine = Engine::new();

        // Set A1 = 10 at tick 0
        session.set_cell(Coord { row: 0, col: 0 }, CellContent::Number(10));

        // Compute at tick 0
        compute_grid_at_tick(&session, &mut engine, 0).unwrap();

        // Verify A1 = 10
        let a1_value = engine.state_curr.get(&Coord { row: 0, col: 0 }).unwrap();
        assert_eq!(*a1_value, Value::Int(10));
    }

    #[test]
    fn test_compute_grid_with_formula() {
        let mut session = CollaborationSession::new();
        let mut engine = Engine::new();

        // Set A1 = 10, B1 = A1 + 5 at tick 0
        session.set_cell(Coord { row: 0, col: 0 }, CellContent::Number(10));
        session.set_cell(Coord { row: 0, col: 1 }, CellContent::Formula("A1 + 5".to_string()));

        // Advance to tick 1 (formulas need a tick to evaluate)
        session.tick();

        // Compute at tick 1
        compute_grid_at_tick(&session, &mut engine, 1).unwrap();

        // Verify B1 = 15
        let b1_value = engine.state_curr.get(&Coord { row: 0, col: 1 }).unwrap();
        assert_eq!(*b1_value, Value::Int(15));
    }

    #[test]
    fn test_concurrent_edits() {
        // Simulate two users editing simultaneously
        let mut session1 = CollaborationSession::new();
        session1.set_peer_id(1).unwrap();
        let mut session2 = CollaborationSession::new();
        session2.set_peer_id(2).unwrap();

        // User 1 edits A1
        session1.set_cell(Coord { row: 0, col: 0 }, CellContent::Number(10));

        // User 2 edits B1
        session2.set_cell(Coord { row: 0, col: 1 }, CellContent::Number(20));

        // Export from both
        let data1 = session1.export_updates().unwrap();
        let data2 = session2.export_updates().unwrap();

        // Import into each other
        session1.import_updates(&data2).unwrap();
        session2.import_updates(&data1).unwrap();

        // Both should have both edits
        let inputs1 = session1.get_inputs_at_tick(0);
        let inputs2 = session2.get_inputs_at_tick(0);
        assert_eq!(inputs1.len(), 2);
        assert_eq!(inputs2.len(), 2);
    }
}
