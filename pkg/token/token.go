package token

// Token represents a COBOL token type
type Token int

// Position represents a position in the source code
type Position struct {
	Line   int
	Column int
	Offset int
}

// TokenInfo contains information about a token
type TokenInfo struct {
	Type    Token
	Literal string
	Pos     Position
}

const (
	// Special tokens
	ILLEGAL Token = iota
	EOF
	COMMENT
	WS // Whitespace

	// COBOL divisions
	IDENTIFICATION_DIVISION
	ENVIRONMENT_DIVISION
	DATA_DIVISION
	PROCEDURE_DIVISION

	// Keywords
	PROGRAM_ID
	AUTHOR
	DATE_WRITTEN
	CONFIGURATION
	SOURCE_COMPUTER
	OBJECT_COMPUTER
	WORKING_STORAGE
	LOCAL_STORAGE
	LINKAGE
	FILE
	SECTION
	PIC
	COMP
	USAGE
	VALUE

	// Data description
	LEVEL_NUMBER // 01-49, 66, 77, 88
	REDEFINES
	OCCURS
	TIMES
	TO
	DEPENDING
	ON
	INDEXED
	BY_KEYWORD
	ASCENDING
	DESCENDING
	KEY
	IS

	// Figurative constants
	ZERO
	ZEROS
	ZEROES
	SPACE
	SPACES
	HIGH_VALUE
	HIGH_VALUES
	LOW_VALUE
	LOW_VALUES
	QUOTE
	QUOTES
	ALL
	NULL
	NULLS

	// Special registers
	LENGTH_OF
	ADDRESS_OF
	RETURN_CODE
	WHEN_COMPILED
	TALLY

	// Verbs
	ACCEPT
	ADD
	CALL
	COMPUTE
	DISPLAY
	DIVIDE
	MOVE
	MULTIPLY
	PERFORM
	STOP
	SUBTRACT
	INITIALIZE
	SET
	GO
	TO_PROC // TO for procedures
	THRU
	VARYING
	FROM
	BY
	UNTIL

	// Conditions and relations
	IF
	ELSE
	END_IF
	EVALUATE
	WHEN
	END_EVALUATE
	GREATER
	LESS
	EQUAL_TO
	GREATER_THAN
	LESS_THAN
	NOT
	AND
	OR
	THAN
	TRUE
	FALSE

	// Literals
	NUMERIC
	STRING_LIT
	DECIMAL_POINT
	CURRENCY

	// Punctuation
	DOT       // .
	COMMA     // ,
	SEMICOLON // ;
	LPAREN    // (
	RPAREN    // )
	LBRACE    // {
	RBRACE    // }

	// Operators
	PLUS     // +
	MINUS    // -
	ASTERISK // *
	SLASH    // /
	EQUAL    // =

	// Identifiers
	IDENT

	// File control tokens
	ASSIGN
	ORGANIZATION
	ACCESS
	SEQUENTIAL
	RANDOM
	DYNAMIC

	// Statement end tokens
	END_PERFORM

	// Exception handling tokens
	TRY
	CATCH
	END_TRY
	FINALLY
	RAISE
	SIZE_ERROR
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",
	COMMENT: "COMMENT",
	WS:      "WS",

	// Divisions
	IDENTIFICATION_DIVISION: "IDENTIFICATION DIVISION",
	ENVIRONMENT_DIVISION:    "ENVIRONMENT DIVISION",
	DATA_DIVISION:           "DATA DIVISION",
	PROCEDURE_DIVISION:      "PROCEDURE DIVISION",

	// Keywords
	PROGRAM_ID:      "PROGRAM-ID",
	AUTHOR:          "AUTHOR",
	DATE_WRITTEN:    "DATE-WRITTEN",
	CONFIGURATION:   "CONFIGURATION",
	SOURCE_COMPUTER: "SOURCE-COMPUTER",
	OBJECT_COMPUTER: "OBJECT-COMPUTER",
	WORKING_STORAGE: "WORKING-STORAGE",
	LOCAL_STORAGE:   "LOCAL-STORAGE",
	LINKAGE:         "LINKAGE",
	FILE:            "FILE",
	SECTION:         "SECTION",
	PIC:             "PIC",
	COMP:            "COMP",
	USAGE:           "USAGE",
	VALUE:           "VALUE",

	// Data description
	LEVEL_NUMBER: "LEVEL_NUMBER",
	REDEFINES:    "REDEFINES",
	OCCURS:       "OCCURS",
	TIMES:        "TIMES",
	TO:           "TO",
	DEPENDING:    "DEPENDING",
	ON:           "ON",
	INDEXED:      "INDEXED",
	BY_KEYWORD:   "BY",
	ASCENDING:    "ASCENDING",
	DESCENDING:   "DESCENDING",
	KEY:          "KEY",
	IS:           "IS",

	// Figurative constants
	ZERO:        "ZERO",
	ZEROS:       "ZEROS",
	ZEROES:      "ZEROES",
	SPACE:       "SPACE",
	SPACES:      "SPACES",
	HIGH_VALUE:  "HIGH-VALUE",
	HIGH_VALUES: "HIGH-VALUES",
	LOW_VALUE:   "LOW-VALUE",
	LOW_VALUES:  "LOW-VALUES",
	QUOTE:       "QUOTE",
	QUOTES:      "QUOTES",
	ALL:         "ALL",
	NULL:        "NULL",
	NULLS:       "NULLS",

	// Special registers
	LENGTH_OF:     "LENGTH OF",
	ADDRESS_OF:    "ADDRESS OF",
	RETURN_CODE:   "RETURN-CODE",
	WHEN_COMPILED: "WHEN-COMPILED",
	TALLY:         "TALLY",

	// Verbs
	ACCEPT:     "ACCEPT",
	ADD:        "ADD",
	CALL:       "CALL",
	COMPUTE:    "COMPUTE",
	DISPLAY:    "DISPLAY",
	DIVIDE:     "DIVIDE",
	MOVE:       "MOVE",
	MULTIPLY:   "MULTIPLY",
	PERFORM:    "PERFORM",
	STOP:       "STOP",
	SUBTRACT:   "SUBTRACT",
	INITIALIZE: "INITIALIZE",
	SET:        "SET",
	GO:         "GO",
	TO_PROC:    "TO",
	THRU:       "THRU",
	VARYING:    "VARYING",
	FROM:       "FROM",
	BY:         "BY",
	UNTIL:      "UNTIL",

	// Conditions
	IF:           "IF",
	ELSE:         "ELSE",
	END_IF:       "END-IF",
	EVALUATE:     "EVALUATE",
	WHEN:         "WHEN",
	END_EVALUATE: "END-EVALUATE",
	GREATER:      "GREATER",
	LESS:         "LESS",
	EQUAL_TO:     "EQUAL TO",
	GREATER_THAN: "GREATER THAN",
	LESS_THAN:    "LESS THAN",
	NOT:          "NOT",
	AND:          "AND",
	OR:           "OR",
	THAN:         "THAN",
	TRUE:         "TRUE",
	FALSE:        "FALSE",

	// Literals
	NUMERIC:       "NUMERIC",
	STRING_LIT:    "STRING",
	DECIMAL_POINT: ".",
	CURRENCY:      "$",

	// Punctuation
	DOT:       ".",
	COMMA:     ",",
	SEMICOLON: ";",
	LPAREN:    "(",
	RPAREN:    ")",
	LBRACE:    "{",
	RBRACE:    "}",

	// Operators
	PLUS:     "+",
	MINUS:    "-",
	ASTERISK: "*",
	SLASH:    "/",
	EQUAL:    "=",

	IDENT: "IDENT",

	// File control tokens
	ASSIGN:       "ASSIGN",
	ORGANIZATION: "ORGANIZATION",
	ACCESS:       "ACCESS",
	SEQUENTIAL:   "SEQUENTIAL",
	RANDOM:       "RANDOM",
	DYNAMIC:      "DYNAMIC",

	// Statement end tokens
	END_PERFORM: "END-PERFORM",

	// Exception handling tokens
	TRY:        "TRY",
	CATCH:      "CATCH",
	END_TRY:    "END-TRY",
	FINALLY:    "FINALLY",
	RAISE:      "RAISE",
	SIZE_ERROR: "SIZE ERROR",
}

// String returns the string representation of the token
func (tok Token) String() string {
	if tok >= 0 && int(tok) < len(tokens) {
		return tokens[tok]
	}
	return ""
}

// Lookup returns the token type for a given identifier
var keywords = map[string]Token{
	"IDENTIFICATION":  IDENTIFICATION_DIVISION,
	"ENVIRONMENT":     ENVIRONMENT_DIVISION,
	"DATA":            DATA_DIVISION,
	"PROCEDURE":       PROCEDURE_DIVISION,
	"DIVISION":        ILLEGAL, // Used as part of other keywords
	"PROGRAM-ID":      PROGRAM_ID,
	"AUTHOR":          AUTHOR,
	"DATE-WRITTEN":    DATE_WRITTEN,
	"CONFIGURATION":   CONFIGURATION,
	"SOURCE-COMPUTER": SOURCE_COMPUTER,
	"OBJECT-COMPUTER": OBJECT_COMPUTER,
	"WORKING-STORAGE": WORKING_STORAGE,
	"LOCAL-STORAGE":   LOCAL_STORAGE,
	"LINKAGE":         LINKAGE,
	"FILE":            FILE,
	"SECTION":         SECTION,
	"PIC":             PIC,
	"PICTURE":         PIC,
	"COMP":            COMP,
	"COMPUTATIONAL":   COMP,
	"USAGE":           USAGE,
	"VALUE":           VALUE,
	"REDEFINES":       REDEFINES,
	"OCCURS":          OCCURS,
	"TIMES":           TIMES,
	"TO":              TO,
	"DEPENDING":       DEPENDING,
	"ON":              ON,
	"INDEXED":         INDEXED,
	"BY":              BY_KEYWORD,
	"ASCENDING":       ASCENDING,
	"DESCENDING":      DESCENDING,
	"KEY":             KEY,
	"IS":              IS,
	"ZERO":            ZERO,
	"ZEROS":           ZEROS,
	"ZEROES":          ZEROES,
	"SPACE":           SPACE,
	"SPACES":          SPACES,
	"HIGH-VALUE":      HIGH_VALUE,
	"HIGH-VALUES":     HIGH_VALUES,
	"LOW-VALUE":       LOW_VALUE,
	"LOW-VALUES":      LOW_VALUES,
	"QUOTE":           QUOTE,
	"QUOTES":          QUOTES,
	"ALL":             ALL,
	"NULL":            NULL,
	"NULLS":           NULLS,
	"LENGTH":          LENGTH_OF,
	"OF":              ILLEGAL, // Used as part of other keywords
	"ADDRESS":         ADDRESS_OF,
	"RETURN-CODE":     RETURN_CODE,
	"WHEN-COMPILED":   WHEN_COMPILED,
	"TALLY":           TALLY,
	"ACCEPT":          ACCEPT,
	"ADD":             ADD,
	"CALL":            CALL,
	"COMPUTE":         COMPUTE,
	"DISPLAY":         DISPLAY,
	"DIVIDE":          DIVIDE,
	"MOVE":            MOVE,
	"MULTIPLY":        MULTIPLY,
	"PERFORM":         PERFORM,
	"STOP":            STOP,
	"SUBTRACT":        SUBTRACT,
	"INITIALIZE":      INITIALIZE,
	"SET":             SET,
	"GO":              GO,
	"THRU":            THRU,
	"VARYING":         VARYING,
	"FROM":            FROM,
	"UNTIL":           UNTIL,
	"IF":              IF,
	"ELSE":            ELSE,
	"END-IF":          END_IF,
	"EVALUATE":        EVALUATE,
	"WHEN":            WHEN,
	"END-EVALUATE":    END_EVALUATE,
	"GREATER":         GREATER,
	"LESS":            LESS,
	"EQUAL":           EQUAL_TO,
	"NOT":             NOT,
	"AND":             AND,
	"OR":              OR,
	"THAN":            THAN,
	"TRUE":            TRUE,
	"FALSE":           FALSE,
	"END-PERFORM":     END_PERFORM,
	"TRY":             TRY,
	"CATCH":           CATCH,
	"END-TRY":         END_TRY,
	"FINALLY":         FINALLY,
	"RAISE":           RAISE,
	"SIZE":            SIZE_ERROR,
}

// IsLevelNumber checks if a string represents a valid COBOL level number
func IsLevelNumber(s string) bool {
	// Valid level numbers are 01-49, 66, 77, 88
	switch s {
	case "66", "77", "88":
		return true
	default:
		if len(s) == 2 {
			if s[0] == '0' || s[0] == '1' || s[0] == '2' || s[0] == '3' || s[0] == '4' {
				if s[1] >= '0' && s[1] <= '9' {
					return true
				}
			}
		}
		return false
	}
}

// Lookup determines if an identifier is a keyword
func Lookup(ident string) Token {
	if tok, isKeyword := keywords[ident]; isKeyword {
		return tok
	}
	if IsLevelNumber(ident) {
		return LEVEL_NUMBER
	}
	return IDENT
}
