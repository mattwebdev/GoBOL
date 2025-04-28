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
	BINARY_CHAR Token = iota + 200
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
	NATIONAL_TYPE
	PACKED_DECIMAL_TYPE
	DISPLAY_1_TYPE

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
	// Additional file sharing options
	SHARING_ALL
	SHARING_READ
	SHARING_WRITE
	SHARING_NONE
	SHARING_DENY_NONE
	SHARING_DENY_READ
	SHARING_DENY_WRITE
	SHARING_DENY_ALL

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
	// Additional report writer control items
	REPORT_HEADING
	PAGE_HEADING
	CONTROL_HEADING
	CONTROL_FOOTING
	DETAIL_LINE
	PAGE_FOOTING
	REPORT_FOOTING
	// Additional report writer group indicators
	GROUP_INDICATE_1 Token = iota + 950
	GROUP_INDICATE_2
	GROUP_INDICATE_3
	GROUP_INDICATE_4
	GROUP_INDICATE_5
	GROUP_INDICATE_6
	GROUP_INDICATE_7
	GROUP_INDICATE_8
	GROUP_INDICATE_9

	// ISO standard special registers (starting at 1100)
	DEBUG_ITEM Token = iota + 1100
	DEBUG_LINE
	DEBUG_NAME
	DEBUG_SUB_1
	DEBUG_SUB_2
	DEBUG_SUB_3
	DEBUG_CONTENTS
	DEBUGGING

	// ISO standard class conditions (starting at 1200)
	CLASS_ALPHABETIC Token = iota + 1200
	CLASS_ALPHANUMERIC
	CLASS_NUMERIC
	CLASS_BOOLEAN
	CLASS_OBJECT
	CLASS_REFERENCE

	// ISO standard sign conditions (starting at 1300)
	SIGN_LEADING Token = iota + 1300
	SIGN_TRAILING
	SIGN_SEPARATE

	// ISO standard currency handling (starting at 1400)
	CURRENCY_SIGN Token = iota + 1400
	CURRENCY_SYMBOL

	// ISO standard decimal handling (starting at 1500)
	DECIMAL_POINT_IS_COMMA Token = iota + 1500
	DECIMAL_POINT_IS_PERIOD

	// Communication tokens
	COMMUNICATION_SECTION
	CD
	SYMBOLIC_QUEUE
	SYMBOLIC_SUB_QUEUE
	MESSAGE_COUNT
	MESSAGE_DATE
	MESSAGE_TIME
	TEXT_LENGTH
	END_KEY
	STATUS_KEY

	// Screen tokens (excluding SCREEN_SECTION which is already defined)
	BLANK_SCREEN
	BLANK_LINE
	BELL
	BLINK
	HIGHLIGHT
	LOWLIGHT
	REVERSE_VIDEO
	UNDERLINE
	PROMPT
	SECURE
	AUTO
	FULL
	REQUIRED

	// Table handling tokens (excluding OCCURS which is already defined)
	DEPENDING_ON
	ASCENDING
	DESCENDING
	INDEXED_BY
	KEY_IS
)

var (
	tokens = make(map[Token]string)
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
	// Data type tokens
	NATIONAL_TYPE:       "NATIONAL",
	PACKED_DECIMAL_TYPE: "PACKED-DECIMAL",
	DISPLAY_1_TYPE:      "DISPLAY-1",
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
	UNDERFLOW:        "UNDERFLOW",
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

	// Additional file sharing options
	SHARING_ALL:        "SHARING ALL",
	SHARING_READ:       "SHARING READ",
	SHARING_WRITE:      "SHARING WRITE",
	SHARING_NONE:       "SHARING NONE",
	SHARING_DENY_NONE:  "SHARING DENY NONE",
	SHARING_DENY_READ:  "SHARING DENY READ",
	SHARING_DENY_WRITE: "SHARING DENY WRITE",
	SHARING_DENY_ALL:   "SHARING DENY ALL",

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

	// Additional report writer control items
	REPORT_HEADING:  "REPORT HEADING",
	PAGE_HEADING:    "PAGE HEADING",
	CONTROL_HEADING: "CONTROL HEADING",
	CONTROL_FOOTING: "CONTROL FOOTING",
	DETAIL_LINE:     "DETAIL LINE",
	PAGE_FOOTING:    "PAGE FOOTING",
	REPORT_FOOTING:  "REPORT FOOTING",

	// Additional report writer group indicators
	GROUP_INDICATE_1: "GROUP INDICATE 1",
	GROUP_INDICATE_2: "GROUP INDICATE 2",
	GROUP_INDICATE_3: "GROUP INDICATE 3",
	GROUP_INDICATE_4: "GROUP INDICATE 4",
	GROUP_INDICATE_5: "GROUP INDICATE 5",
	GROUP_INDICATE_6: "GROUP INDICATE 6",
	GROUP_INDICATE_7: "GROUP INDICATE 7",
	GROUP_INDICATE_8: "GROUP INDICATE 8",
	GROUP_INDICATE_9: "GROUP INDICATE 9",

	// ISO standard special registers
	DEBUG_ITEM:     "DEBUG-ITEM",
	DEBUG_LINE:     "DEBUG-LINE",
	DEBUG_NAME:     "DEBUG-NAME",
	DEBUG_SUB_1:    "DEBUG-SUB-1",
	DEBUG_SUB_2:    "DEBUG-SUB-2",
	DEBUG_SUB_3:    "DEBUG-SUB-3",
	DEBUG_CONTENTS: "DEBUG-CONTENTS",
	DEBUGGING:      "DEBUGGING",

	// ISO standard class conditions
	CLASS_ALPHABETIC:   "CLASS-ALPHABETIC",
	CLASS_ALPHANUMERIC: "CLASS-ALPHANUMERIC",
	CLASS_NUMERIC:      "CLASS-NUMERIC",
	CLASS_BOOLEAN:      "CLASS-BOOLEAN",
	CLASS_OBJECT:       "CLASS-OBJECT",
	CLASS_REFERENCE:    "CLASS-REFERENCE",

	// ISO standard sign conditions
	SIGN_LEADING:  "SIGN-LEADING",
	SIGN_TRAILING: "SIGN-TRAILING",
	SIGN_SEPARATE: "SIGN-SEPARATE",

	// ISO standard currency handling
	CURRENCY_SIGN:   "CURRENCY-SIGN",
	CURRENCY_SYMBOL: "CURRENCY-SYMBOL",

	// ISO standard decimal handling
	DECIMAL_POINT_IS_COMMA:  "DECIMAL-POINT-IS-COMMA",
	DECIMAL_POINT_IS_PERIOD: "DECIMAL-POINT-IS-PERIOD",
}

// String returns the string representation of a token
func (t Token) String() string {
	if str, ok := tokenStrings[t]; ok {
		return str
	}
	return "TOKEN(" + strconv.Itoa(int(t)) + ")"
}

func init() {
	tokens[NATIONAL_TYPE] = "NATIONAL"
	tokens[PACKED_DECIMAL_TYPE] = "PACKED-DECIMAL"
	tokens[DISPLAY_1_TYPE] = "DISPLAY-1"
	tokens[REPORT_HEADING] = "REPORT HEADING"
}
