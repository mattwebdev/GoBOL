# GoBOL Token Package Documentation

## Overview

The `pkg/token` package provides the foundational token system for the GoBOL compiler. It defines and manages all tokens used in COBOL source code, including their classifications, relationships, and semantic information.

## Package Structure

```
pkg/token/
├── types.go          # Core token type definitions and string mappings
├── keywords.go       # Keyword token definitions and information
├── verbs.go         # COBOL verb token definitions
├── conditions.go    # Condition token definitions
├── datatypes.go     # Data type token definitions
├── fileorg.go       # File organization token definitions
├── report.go        # Report writer token definitions
├── scope.go         # Scope-related token definitions
├── position.go      # Source position tracking
└── info.go          # Token information interfaces
```

## Core Components

### 1. Token Type (`types.go`)

The fundamental token type definition and mapping system.

```go
type Token int

const (
    // Special tokens
    ILLEGAL Token = iota
    EOF
    COMMENT
    IDENTIFIER
    STRING_LIT
    NUMBER_LIT
    LEVEL_NUMBER

    // Token categories follow...
)
```

Features:
- Unique integer values for each token
- String representation mapping
- Token classification system
- Token range management

### 2. Keywords (`keywords.go`)

Manages COBOL keyword tokens and their properties.

```go
type KeywordInfo struct {
    Token    Token
    Class    TokenClass
    Aliases  []string
    Context  []Token
    Category string
}
```

Features:
- Keyword categorization
- Alias support (e.g., THRU/THROUGH)
- Context-aware keyword information
- Category-based organization

### 3. Verbs (`verbs.go`)

Handles COBOL verb tokens and their characteristics.

```go
type VerbInfo struct {
    Token       Token
    Class       TokenClass
    Category    string
    Terminators []Token
}
```

Features:
- Verb categorization
- Scope terminator association
- Usage context tracking
- Verb-specific properties

### 4. Conditions (`conditions.go`)

Manages condition-related tokens and logic.

```go
type ConditionInfo struct {
    Token    Token
    Class    TokenClass
    Category string
    Negation Token
}
```

Features:
- Condition type classification
- Negation handling
- Condition relationships
- Context validation

### 5. Data Types (`datatypes.go`)

Defines COBOL data type tokens and their properties.

```go
type DataTypeInfo struct {
    Token    Token
    Class    TokenClass
    Category string
    Size     int
    Usage    string
}
```

Features:
- Data type classification
- Size information
- Usage specifications
- Type compatibility

### 6. File Organization (`fileorg.go`)

Handles file-related tokens and organizations.

```go
type FileOrgInfo struct {
    Token    Token
    Class    TokenClass
    Category string
    Access   []Token
}
```

Features:
- File organization types
- Access method association
- File handling properties
- Organization validation

### 7. Report Writer (`report.go`)

Manages report writer specific tokens.

```go
type ReportInfo struct {
    Token    Token
    Class    TokenClass
    Category string
    Usage    string
}
```

Features:
- Report section tokens
- Control break handling
- Page formatting
- Report writer specifics

### 8. Position Tracking (`position.go`)

Handles source code position information.

```go
type Position struct {
    Filename     string
    Line         int
    Column       int
    Offset       int
    Area         Area
    SeqNum       int
    Continuation bool
}
```

Features:
- File location tracking
- COBOL-specific area tracking
- Sequence number support
- Continuation line handling

## Token Classification System

### Token Classes
```go
type TokenClass int

const (
    CLASS_SPECIAL TokenClass = iota
    CLASS_KEYWORD
    CLASS_VERB
    CLASS_LITERAL
    CLASS_IDENTIFIER
    CLASS_SEPARATOR
    CLASS_OPERATOR
    CLASS_MODIFIER
)
```

### Token Categories
- Arithmetic
- Comparison
- Control Flow
- Data Description
- Environment
- File Handling
- Procedure
- Report Writer

## Usage Examples

### 1. Token Creation and Identification
```go
// Check if a token is a keyword
if IsKeyword(token) {
    info, exists := GetKeywordInfo(token)
    if exists {
        // Process keyword...
    }
}

// Get verb information
if info, exists := GetVerbInfo(token); exists {
    // Process verb...
}
```

### 2. Position Handling
```go
pos := NewPosition("source.cbl", 10, 1, 100)
pos = pos.WithSequenceNumber(1000)
pos = pos.WithContinuation(true)
```

### 3. Token Information Retrieval
```go
// Get data type information
if info, exists := GetDataTypeInfo(token); exists {
    size := info.Size
    usage := info.Usage
}

// Get report writer information
if info, exists := GetReportInfo(token); exists {
    category := info.Category
}
```

## Token Ranges

The token system uses specific ranges for different token types:

```go
// Basic tokens: 0-99
// Data types: 100-199
// Verbs: 200-399
// Scope terminators: 400-499
// Conditions: 500-599
// Keywords: 600-799
// File organization: 800-849
// ISO keywords: 850-899
// Report writer: 900-999
```

## Testing

Each component has corresponding test files:

```
pkg/token/
├── types_test.go
├── keywords_test.go
├── verbs_test.go
├── conditions_test.go
├── datatypes_test.go
├── fileorg_test.go
├── report_test.go
├── scope_test.go
└── position_test.go
```

Test coverage includes:
- Token creation and identification
- Information retrieval
- Position handling
- Edge cases
- Error conditions

## Performance Considerations

1. Token Lookup
   - O(1) map-based lookups
   - Cached information
   - Minimal allocations

2. Position Tracking
   - Efficient updates
   - Minimal memory usage
   - Quick comparisons

3. Information Storage
   - Static maps
   - Const declarations
   - Zero-allocation design

## Future Enhancements

1. Extended Support
   - Additional COBOL dialects
   - Custom token extensions
   - Enhanced token metadata

2. Performance Improvements
   - Token pooling
   - Cached relationships
   - Optimized lookups

3. Additional Features
   - Token transformation rules
   - Enhanced validation
   - Context-aware suggestions

## Integration Points

1. Lexer Integration
   - Token recognition
   - Position tracking
   - Error reporting

2. Parser Integration
   - Token validation
   - Context checking
   - Scope management

3. Semantic Analysis
   - Token relationships
   - Type checking
   - Scope validation

## Best Practices

1. Token Usage
   - Use constants over raw values
   - Check token validity
   - Handle unknown tokens

2. Position Handling
   - Track all source positions
   - Maintain area information
   - Handle continuations properly

3. Information Retrieval
   - Check existence before use
   - Handle missing information
   - Use type-specific getters
