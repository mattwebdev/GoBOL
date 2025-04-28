package token

import "strconv"

// Token represents a COBOL token type
type Token int

// TokenType is an alias for Token to maintain compatibility
type TokenType = Token

const (
	// Special tokens
	ILLEGAL Token = iota
	EOF
	COMMENT
	IDENTIFIER
	STRING_LIT   // String literal token
	NUMBER_LIT   // Number literal token
	LEVEL_NUMBER // COBOL level number (01-49, 66, 77, 88)

	// Data type tokens
	BINARY_CHAR
	BINARY_SHORT
	BINARY_LONG
	BINARY_DOUBLE
	FLOAT_SHORT
	FLOAT_LONG
	FLOAT_EXTENDED
	POINTER_32
	PROCEDURE_POINTER
	OBJECT
	REFERENCE

	// Additional scope terminators
	END_START
	END_DELETE
	END_REWRITE
	END_RETURN
	END_INITIALIZE

	// Additional condition keywords
	ON_EXCEPTION
	NOT_ON_EXCEPTION
	ON_OVERFLOW
	NOT_ON_OVERFLOW
	ON_SIZE_ERROR
	NOT_ON_SIZE_ERROR

	// Additional file organization keywords
	LINE_SEQUENTIAL
	QUEUE
	RELATIVE_KEY
	RECORD_KEY

	// Report Writer tokens (starting at 900)
	INITIATE Token = iota + 900
	TERMINATE
	LINE_COUNTER
	PAGE_COUNTER
	NEXT_GROUP
	NEXT_PAGE
	DE
	RH
	PH
	RF
	PF
	CH
	CF
	GROUP_INDICATE
)

// tokenStrings maps tokens to their string representations
var tokenStrings = map[Token]string{
	ILLEGAL:      "ILLEGAL",
	EOF:          "EOF",
	COMMENT:      "COMMENT",
	IDENTIFIER:   "IDENTIFIER",
	STRING_LIT:   "STRING_LIT",
	NUMBER_LIT:   "NUMBER_LIT",
	LEVEL_NUMBER: "LEVEL_NUMBER",
	// Verb tokens
	MOVE:         "MOVE",
	ADD:          "ADD",
	SUBTRACT:     "SUBTRACT",
	MULTIPLY:     "MULTIPLY",
	DIVIDE:       "DIVIDE",
	COMPUTE:      "COMPUTE",
	INITIALIZE:   "INITIALIZE",
	ACCEPT:       "ACCEPT",
	DISPLAY:      "DISPLAY",
	READ:         "READ",
	WRITE:        "WRITE",
	REWRITE:      "REWRITE",
	DELETE:       "DELETE",
	START:        "START",
	OPEN:         "OPEN",
	CLOSE:        "CLOSE",
	RELEASE:      "RELEASE",
	RETURN:       "RETURN",
	PERFORM:      "PERFORM",
	GO_TO:        "GO_TO",
	IF:           "IF",
	EVALUATE:     "EVALUATE",
	CONTINUE:     "CONTINUE",
	EXIT:         "EXIT",
	STOP:         "STOP",
	ALTER:        "ALTER",
	USE:          "USE",
	STRING_VERB:  "STRING",
	UNSTRING:     "UNSTRING",
	INSPECT:      "INSPECT",
	SEARCH:       "SEARCH",
	SET:          "SET",
	SORT:         "SORT",
	MERGE:        "MERGE",
	GENERATE:     "GENERATE",
	SUPPRESS:     "SUPPRESS",
	CALL:         "CALL",
	CANCEL:       "CANCEL",
	GOBACK:       "GOBACK",
	EXIT_PROGRAM: "EXIT_PROGRAM",
	// Additional verbs
	RAISE:    "RAISE",
	RESUME:   "RESUME",
	ALLOCATE: "ALLOCATE",
	FREE:     "FREE",
	VALIDATE: "VALIDATE",
	// Figurative constants
	ZERO:        "ZERO",
	ZEROS:       "ZEROS",
	ZEROES:      "ZEROES",
	SPACE:       "SPACE",
	SPACES:      "SPACES",
	HIGH_VALUE:  "HIGH_VALUE",
	HIGH_VALUES: "HIGH_VALUES",
	LOW_VALUE:   "LOW_VALUE",
	LOW_VALUES:  "LOW_VALUES",
	QUOTE:       "QUOTE",
	QUOTES:      "QUOTES",
	NULL:        "NULL",
	NULLS:       "NULLS",
	ALL:         "ALL",
	// Keywords
	CORRESPONDING: "CORRESPONDING",
	CORR:          "CORR",
	WHEN:          "WHEN",
	THROUGH:       "THROUGH",
	THRU:          "THRU",
	AFTER:         "AFTER",
	BEFORE:        "BEFORE",
	STANDARD:      "STANDARD",
	BASED:         "BASED",
	BINARY:        "BINARY",
	COMPUTATIONAL: "COMPUTATIONAL",
	COMP:          "COMP",
	COMP_1:        "COMP-1",
	COMP_2:        "COMP-2",
	COMP_3:        "COMP-3",
	COMP_4:        "COMP-4",
	COMP_5:        "COMP-5",
	SYNCHRONIZED:  "SYNCHRONIZED",
	SYNC:          "SYNC",
	JUSTIFIED:     "JUSTIFIED",
	JUST:          "JUST",
	BLANK:         "BLANK",
	RENAMES:       "RENAMES",
	REDEFINES:     "REDEFINES",
	FILLER:        "FILLER",
	END_IF:        "END-IF",
	END_READ:      "END-READ",
	END_WRITE:     "END-WRITE",
	END_PERFORM:   "END-PERFORM",
	END_EVALUATE:  "END-EVALUATE",
	END_SEARCH:    "END-SEARCH",
	END_COMPUTE:   "END-COMPUTE",
	END_ADD:       "END-ADD",
	END_SUBTRACT:  "END-SUBTRACT",
	END_MULTIPLY:  "END-MULTIPLY",
	END_DIVIDE:    "END-DIVIDE",
	END_STRING:    "END-STRING",
	END_UNSTRING:  "END-UNSTRING",
	END_CALL:      "END-CALL",
	END_ACCEPT:    "END-ACCEPT",
	END_DISPLAY:   "END-DISPLAY",

	// File-related keywords
	ORGANIZATION: "ORGANIZATION",
	SEQUENTIAL:   "SEQUENTIAL",
	RELATIVE:     "RELATIVE",
	ACCESS:       "ACCESS",
	RANDOM:       "RANDOM",
	DYNAMIC:      "DYNAMIC",
	FILE:         "FILE",
	STATUS:       "STATUS",
	RECORD:       "RECORD",
	KEY:          "KEY",
	ALTERNATE:    "ALTERNATE",
	LABEL:        "LABEL",
	BLOCK:        "BLOCK",
	CONTAINS:     "CONTAINS",
	RECORDING:    "RECORDING",
	MODE:         "MODE",

	// Additional file-related keywords
	LINE:             "LINE",
	ADVANCING:        "ADVANCING",
	OPTIONAL:         "OPTIONAL",
	LINAGE:           "LINAGE",
	FOOTING:          "FOOTING",
	TOP:              "TOP",
	BOTTOM:           "BOTTOM",
	PADDING:          "PADDING",
	CHARACTER:        "CHARACTER",
	DATA_RECORD:      "DATA RECORD",
	CODE_SET:         "CODE-SET",
	SHARING:          "SHARING",
	WITH:             "WITH",
	NO:               "NO",
	OTHER:            "OTHER",
	LOCK:             "LOCK",
	AUTOMATIC:        "AUTOMATIC",
	MANUAL:           "MANUAL",
	EXCLUSIVE:        "EXCLUSIVE",
	RETRY:            "RETRY",
	REPORT:           "REPORT",
	PAGE:             "PAGE",
	HEADING:          "HEADING",
	FIRST:            "FIRST",
	LAST:             "LAST",
	DETAIL:           "DETAIL",
	CONTROL:          "CONTROL",
	FINAL:            "FINAL",
	SUM:              "SUM",
	RESET:            "RESET",
	UNDERFLOW:        "UNDERFLOW",
	REMAINDER:        "REMAINDER",
	POINTER:          "POINTER",
	INDEX:            "INDEX",
	NATIONAL:         "NATIONAL",
	INVALID_KEY:      "INVALID KEY",
	AT_END:           "AT END",
	SIGN:             "SIGN",
	SEPARATE:         "SEPARATE",
	PACKED_DECIMAL:   "PACKED-DECIMAL",
	DISPLAY_1:        "DISPLAY-1",
	GLOBAL:           "GLOBAL",
	EXTERNAL:         "EXTERNAL",
	NUMERIC:          "NUMERIC",
	ALPHABETIC:       "ALPHABETIC",
	ALPHABETIC_LOWER: "ALPHABETIC-LOWER",
	ALPHABETIC_UPPER: "ALPHABETIC-UPPER",
	CLASS:            "CLASS",
	POSITIVE:         "POSITIVE",
	NEGATIVE:         "NEGATIVE",
	DECIMAL_POINT:    "DECIMAL-POINT",
	CURRENCY:         "CURRENCY",
	CONSOLE:          "CONSOLE",
	PRINTER:          "PRINTER",
	SYSIN:            "SYSIN",
	SYSOUT:           "SYSOUT",
	USING:            "USING",
	RETURNING:        "RETURNING",
	RAISING:          "RAISING",
	EXCEPTION:        "EXCEPTION",
	SIZE:             "SIZE",
	ERROR:            "ERROR",
	OVERFLOW:         "OVERFLOW",
	// Report Writer tokens are now defined in report.go

	// New data type tokens
	BINARY_CHAR:       "BINARY-CHAR",
	BINARY_SHORT:      "BINARY-SHORT",
	BINARY_LONG:       "BINARY-LONG",
	BINARY_DOUBLE:     "BINARY-DOUBLE",
	FLOAT_SHORT:       "FLOAT-SHORT",
	FLOAT_LONG:        "FLOAT-LONG",
	FLOAT_EXTENDED:    "FLOAT-EXTENDED",
	POINTER_32:        "POINTER-32",
	PROCEDURE_POINTER: "PROCEDURE-POINTER",
	OBJECT:            "OBJECT",
	REFERENCE:         "REFERENCE",

	// New scope terminators
	END_START:      "END-START",
	END_DELETE:     "END-DELETE",
	END_REWRITE:    "END-REWRITE",
	END_RETURN:     "END-RETURN",
	END_INITIALIZE: "END-INITIALIZE",

	// New condition keywords
	ON_EXCEPTION:      "ON EXCEPTION",
	NOT_ON_EXCEPTION:  "NOT ON EXCEPTION",
	ON_OVERFLOW:       "ON OVERFLOW",
	NOT_ON_OVERFLOW:   "NOT ON OVERFLOW",
	ON_SIZE_ERROR:     "ON SIZE ERROR",
	NOT_ON_SIZE_ERROR: "NOT ON SIZE ERROR",

	// New file organization keywords
	LINE_SEQUENTIAL: "LINE SEQUENTIAL",
	QUEUE:           "QUEUE",
	RELATIVE_KEY:    "RELATIVE KEY",
	RECORD_KEY:      "RECORD KEY",

	// Report Writer tokens (starting at 900)
	INITIATE:       "INITIATE",
	TERMINATE:      "TERMINATE",
	LINE_COUNTER:   "LINE-COUNTER",
	PAGE_COUNTER:   "PAGE-COUNTER",
	NEXT_GROUP:     "NEXT GROUP",
	NEXT_PAGE:      "NEXT PAGE",
	DE:             "DE",
	RH:             "RH",
	PH:             "PH",
	RF:             "RF",
	PF:             "PF",
	CH:             "CH",
	CF:             "CF",
	GROUP_INDICATE: "GROUP INDICATE",
}

// String returns the string representation of a token
func (t Token) String() string {
	if str, ok := tokenStrings[t]; ok {
		return str
	}
	return "TOKEN(" + strconv.Itoa(int(t)) + ")"
}
