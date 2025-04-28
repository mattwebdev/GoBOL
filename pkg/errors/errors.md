# GoBOL Error Handling System

## Overview

The `pkg/errors` package provides a robust error handling system specifically designed for COBOL compilation and execution. It supports detailed error reporting, error recovery, and helpful diagnostics for COBOL developers.

## Error Categories

### 1. Lexical Errors
- Invalid characters in source code
- Malformed continuation lines
- Area A/B violations
- Invalid picture clauses
- Invalid literals
- Line length violations
- Sequence number errors

### 2. Syntax Errors
- Invalid division structure
- Malformed statements
- Missing required clauses
- Invalid statement order
- Unmatched scope terminators
- Invalid identifier names
- Invalid period placement

### 3. Semantic Errors
- Undefined identifiers
- Type mismatches
- Invalid data declarations
- Incompatible operations
- Duplicate declarations
- Invalid references
- Invalid file operations

### 4. Runtime Errors
- Arithmetic overflow/underflow
- File I/O errors
- Memory allocation errors
- Invalid data values
- Program call errors
- Sort/merge errors
- Report writer errors

## Error Structure

```go
type Error struct {
    Code        ErrorCode   // Unique error identifier
    Category    Category    // Error category (Lexical, Syntax, etc.)
    Position    Position    // Source code position
    Message     string      // Human-readable error message
    Suggestion  string      // Suggested fix
    Context     string      // Relevant source code context
    Severity    Severity    // Error severity level
    Related     []Error     // Related errors
    Recovery    bool        // Whether error recovery is possible
}
```

## Implementation Plan

### Phase 1: Core Error Types
1. Define error codes and categories
2. Implement basic Error structure
3. Create error creation helpers
4. Add position tracking
5. Implement error formatting
6. Add basic error recovery flags

### Phase 2: Enhanced Error Context
1. Add source code context capture
2. Implement suggestion generation
3. Add related error linking
4. Implement severity levels
5. Add error grouping
6. Create error filtering

### Phase 3: Error Reporting
1. Implement console reporter
2. Add JSON output format
3. Create HTML report generator
4. Add error statistics
5. Implement error summarization
6. Add error categorization

### Phase 4: Error Recovery
1. Implement recovery strategies
2. Add error state tracking
3. Create recovery suggestions
4. Implement cascade prevention
5. Add recovery verification
6. Create recovery documentation

## Error Codes

### Format: GBLXXXYYY
- GBL: GoBOL prefix
- XXX: Category identifier
  - LEX: Lexical
  - SYN: Syntax
  - SEM: Semantic
  - RUN: Runtime
- YYY: Specific error number

Example:
```go
const (
    ErrInvalidChar    = "GBL001LEX" // Invalid character in source
    ErrInvalidArea    = "GBL002LEX" // Area A/B violation
    ErrInvalidPeriod  = "GBL001SYN" // Invalid period placement
    ErrUndefinedIdent = "GBL001SEM" // Undefined identifier
)
```

## Error Severity Levels

```go
type Severity int

const (
    Info     Severity = iota // Informational message
    Warning                  // Warning but compilation continues
    Error                    // Error but may continue with recovery
    Fatal                    // Fatal error, compilation stops
)
```

## Error Recovery Strategies

### 1. Lexical Recovery
- Skip to next valid token
- Handle invalid characters
- Correct common character mistakes
- Handle line continuation errors

### 2. Syntax Recovery
- Skip to next statement
- Insert missing periods
- Handle missing scope terminators
- Recover from invalid statement order

### 3. Semantic Recovery
- Use default types
- Create missing declarations
- Handle duplicate identifiers
- Resolve ambiguous references

## Error Reporting Formats

### 1. Console Output
```
Error GBL001LEX: Invalid character '*' in Area A
  --> source.cbl:10:1
   |
10 | *THIS IS A COMMENT
   | ^ Comments must start in Area B (column 7)
   |
   = Suggestion: Move the '*' to column 7
```

### 2. JSON Format
```json
{
  "error": {
    "code": "GBL001LEX",
    "position": {
      "file": "source.cbl",
      "line": 10,
      "column": 1
    },
    "message": "Invalid character '*' in Area A",
    "suggestion": "Move the '*' to column 7",
    "severity": "error"
  }
}
```

## Usage Examples

```go
// Creating a new error
err := errors.New(ErrInvalidChar).
    WithPosition(pos).
    WithSuggestion("Use a valid COBOL character").
    WithContext(sourceContext)

// Error recovery
if err := parser.Parse(); err != nil {
    if errors.IsRecoverable(err) {
        parser.Recover()
        continue
    }
    return err
}
```

## Testing Strategy

1. Unit Tests
   - Test all error creation paths
   - Verify error formatting
   - Test recovery mechanisms
   - Validate error codes

2. Integration Tests
   - Test with lexer/parser
   - Verify error recovery
   - Test error reporting
   - Validate error context

3. Error Corpus
   - Maintain test cases for each error
   - Include recovery scenarios
   - Test error combinations
   - Verify suggestions

## Performance Considerations

1. Error Creation
   - Minimize allocations
   - Cache common errors
   - Optimize context capture
   - Efficient position tracking

2. Error Reporting
   - Lazy formatting
   - Efficient context storage
   - Optimized JSON conversion
   - Smart error grouping

## Future Enhancements

1. IDE Integration
   - LSP error format support
   - Quick fix proposals
   - Error highlighting
   - Error navigation

2. Advanced Features
   - Error pattern recognition
   - Machine learning-based suggestions
   - Historical error tracking
   - Error statistics and analytics

3. Compatibility
   - Support for different COBOL dialects
   - Legacy error code mapping
   - Custom error formatters
   - Error localization
