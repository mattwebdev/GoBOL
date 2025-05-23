package token

// Basic keyword tokens start at 600
const (
	// Basic keywords
	GO Token = iota + 600
	TO
	THAN
	OR
	AND
	IS
	NOT
	IN
	BY
	FROM
	GIVING
	WHEN
	THROUGH
	THRU // Alias for THROUGH
	AFTER
	BEFORE
	STANDARD
	BASED
	// THEN is defined in types.go

	// End-scope keywords
	END_IF
	END_READ
	END_WRITE
	END_PERFORM
	END_EVALUATE
	END_SEARCH
	END_COMPUTE
	END_ADD
	END_SUBTRACT
	END_MULTIPLY
	END_DIVIDE
	END_STRING
	END_UNSTRING
	END_CALL
	END_ACCEPT
	END_DISPLAY

	// Division-related keywords
	IDENTIFICATION
	ENVIRONMENT
	DATA
	PROCEDURE
	DIVISION
	SECTION

	// Data description
	PICTURE
	PIC
	USAGE
	VALUE
	VALUES
	OCCURS
	TIMES
	DEPENDING
	ON
	INDEXED
	VARYING
	UNTIL
	BINARY
	COMPUTATIONAL
	COMP   // Alias for COMPUTATIONAL
	COMP_1 // COMP-1
	COMP_2 // COMP-2
	COMP_3 // COMP-3
	COMP_4 // COMP-4
	COMP_5 // COMP-5
	SYNCHRONIZED
	SYNC // Alias for SYNCHRONIZED
	JUSTIFIED
	JUST // Alias for JUSTIFIED
	BLANK
	RENAMES
	REDEFINES
	FILLER

	// Comparison keywords
	GREATER
	LESS
	EQUAL
	EQUALS
	CORRESPONDING
	CORR // Alias for CORRESPONDING

	// File-related keywords
	ORGANIZATION
	SEQUENTIAL
	RELATIVE
	ACCESS
	RANDOM
	DYNAMIC
	FILE
	STATUS
	RECORD
	KEY
	ALTERNATE
	LABEL
	BLOCK
	CONTAINS
	RECORDING
	MODE

	// Additional file-related keywords start at 800 to avoid collision with verbs
	LINE Token = iota + 800
	ADVANCING
	OPTIONAL
	LINAGE
	FOOTING
	TOP
	BOTTOM
	PADDING
	CHARACTER
	DATA_RECORD
	CODE_SET
	SHARING
	WITH
	NO
	OTHER
	LOCK
	AUTOMATIC
	MANUAL
	EXCLUSIVE
	RETRY
	REPORT
	PAGE
	HEADING
	FIRST
	LAST
	DETAIL
	CONTROL
	FINAL
	SUM
	RESET

	// Additional ISO standard keywords start at 850
	SIGN Token = iota + 850
	SEPARATE
	PACKED_DECIMAL
	DISPLAY_1
	GLOBAL
	EXTERNAL
	NUMERIC
	ALPHABETIC
	ALPHABETIC_LOWER
	ALPHABETIC_UPPER
	CLASS
	POSITIVE
	NEGATIVE
	DECIMAL_POINT
	CURRENCY
	CONSOLE
	PRINTER
	SYSIN
	SYSOUT
	USING
	RETURNING
	RAISING
	EXCEPTION
	SIZE
	ERROR
	OVERFLOW
	UNDERFLOW

	// Additional missing ISO keywords
	REMAINDER   // For DIVIDE operation
	POINTER     // For pointer usage
	INDEX       // For index usage
	NATIONAL    // For national character usage
	INVALID_KEY // For file handling conditions
	AT_END      // For file handling conditions
)

// KeywordInfo contains information about a keyword
type KeywordInfo struct {
	Token    Token
	Class    TokenClass
	Aliases  []string // Alternative spellings/forms
	Context  []Token  // Valid in context of these verbs
	Category string   // Logical category (comparison, modifier, etc)
}

// Define keyword information
var keywordInfos = map[Token]KeywordInfo{
	CORRESPONDING: {
		Token:    CORRESPONDING,
		Class:    CLASS_MODIFIER,
		Aliases:  []string{"CORR"},
		Context:  []Token{GREATER, LESS, EQUAL, EQUALS},
		Category: "modifier",
	},
	TO: {
		Token:    TO,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, FROM, WHEN, AFTER, BEFORE},
		Category: "destination",
	},
	GIVING: {
		Token:    GIVING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO},
		Category: "destination",
	},
	GREATER: {
		Token:    GREATER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{TO, THAN},
		Category: "comparison",
	},
	THAN: {
		Token:    THAN,
		Class:    CLASS_KEYWORD,
		Context:  []Token{TO, GREATER},
		Category: "comparison",
	},
	COMPUTATIONAL: {
		Token:    COMPUTATIONAL,
		Class:    CLASS_KEYWORD,
		Aliases:  []string{"COMP"},
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "usage",
	},
	COMP: {
		Token:    COMP,
		Class:    CLASS_KEYWORD,
		Context:  []Token{COMPUTATIONAL},
		Category: "usage",
	},
	COMP_1: {
		Token:    COMP_1,
		Class:    CLASS_KEYWORD,
		Context:  []Token{COMPUTATIONAL},
		Category: "usage",
	},
	COMP_2: {
		Token:    COMP_2,
		Class:    CLASS_KEYWORD,
		Context:  []Token{COMPUTATIONAL},
		Category: "usage",
	},
	COMP_3: {
		Token:    COMP_3,
		Class:    CLASS_KEYWORD,
		Context:  []Token{COMPUTATIONAL},
		Category: "usage",
	},
	COMP_4: {
		Token:    COMP_4,
		Class:    CLASS_KEYWORD,
		Context:  []Token{COMPUTATIONAL},
		Category: "usage",
	},
	COMP_5: {
		Token:    COMP_5,
		Class:    CLASS_KEYWORD,
		Context:  []Token{COMPUTATIONAL},
		Category: "usage",
	},
	BINARY: {
		Token:    BINARY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{COMPUTATIONAL},
		Category: "usage",
	},
	SYNCHRONIZED: {
		Token:    SYNCHRONIZED,
		Class:    CLASS_KEYWORD,
		Aliases:  []string{"SYNC"},
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "alignment",
	},
	SYNC: {
		Token:    SYNC,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SYNCHRONIZED},
		Category: "alignment",
	},
	JUSTIFIED: {
		Token:    JUSTIFIED,
		Class:    CLASS_KEYWORD,
		Aliases:  []string{"JUST"},
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "alignment",
	},
	JUST: {
		Token:    JUST,
		Class:    CLASS_KEYWORD,
		Context:  []Token{JUSTIFIED},
		Category: "alignment",
	},
	BLANK: {
		Token:    BLANK,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "data_description",
	},
	RENAMES: {
		Token:    RENAMES,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "data_description",
	},
	REDEFINES: {
		Token:    REDEFINES,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "data_description",
	},
	FILLER: {
		Token:    FILLER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "data_description",
	},
	THROUGH: {
		Token:    THROUGH,
		Class:    CLASS_KEYWORD,
		Aliases:  []string{"THRU"},
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "range",
	},
	THRU: {
		Token:    THRU,
		Class:    CLASS_KEYWORD,
		Context:  []Token{THROUGH},
		Category: "range",
	},
	WHEN: {
		Token:    WHEN,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "control",
	},
	AFTER: {
		Token:    AFTER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "position",
	},
	BEFORE: {
		Token:    BEFORE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "position",
	},
	STANDARD: {
		Token:    STANDARD,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "qualifier",
	},
	BASED: {
		Token:    BASED,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "memory",
	},
	END_IF: {
		Token:    END_IF,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_READ: {
		Token:    END_READ,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_WRITE: {
		Token:    END_WRITE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_PERFORM: {
		Token:    END_PERFORM,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_EVALUATE: {
		Token:    END_EVALUATE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_SEARCH: {
		Token:    END_SEARCH,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_COMPUTE: {
		Token:    END_COMPUTE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_ADD: {
		Token:    END_ADD,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_SUBTRACT: {
		Token:    END_SUBTRACT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_MULTIPLY: {
		Token:    END_MULTIPLY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_DIVIDE: {
		Token:    END_DIVIDE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_STRING: {
		Token:    END_STRING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_UNSTRING: {
		Token:    END_UNSTRING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_CALL: {
		Token:    END_CALL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_ACCEPT: {
		Token:    END_ACCEPT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},
	END_DISPLAY: {
		Token:    END_DISPLAY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope_terminator",
	},

	// File-related keyword info
	ORGANIZATION: {
		Token:    ORGANIZATION,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	SEQUENTIAL: {
		Token:    SEQUENTIAL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{ORGANIZATION},
		Category: "file_organization",
	},
	RELATIVE: {
		Token:    RELATIVE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{ORGANIZATION},
		Category: "file_organization",
	},
	ACCESS: {
		Token:    ACCESS,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	RANDOM: {
		Token:    RANDOM,
		Class:    CLASS_KEYWORD,
		Context:  []Token{ACCESS},
		Category: "file_access",
	},
	DYNAMIC: {
		Token:    DYNAMIC,
		Class:    CLASS_KEYWORD,
		Context:  []Token{ACCESS},
		Category: "file_access",
	},
	FILE: {
		Token:    FILE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file",
	},
	STATUS: {
		Token:    STATUS,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	RECORD: {
		Token:    RECORD,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	KEY: {
		Token:    KEY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{RECORD},
		Category: "file_description",
	},
	ALTERNATE: {
		Token:    ALTERNATE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{KEY},
		Category: "file_description",
	},
	LABEL: {
		Token:    LABEL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{RECORD},
		Category: "file_description",
	},
	BLOCK: {
		Token:    BLOCK,
		Class:    CLASS_KEYWORD,
		Context:  []Token{CONTAINS},
		Category: "file_description",
	},
	CONTAINS: {
		Token:    CONTAINS,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	RECORDING: {
		Token:    RECORDING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{MODE},
		Category: "file_description",
	},
	MODE: {
		Token:    MODE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},

	// Add keyword info for new tokens
	LINE: {
		Token:    LINE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	ADVANCING: {
		Token:    ADVANCING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	OPTIONAL: {
		Token:    OPTIONAL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	LINAGE: {
		Token:    LINAGE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	FOOTING: {
		Token:    FOOTING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	TOP: {
		Token:    TOP,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	BOTTOM: {
		Token:    BOTTOM,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	PADDING: {
		Token:    PADDING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	CHARACTER: {
		Token:    CHARACTER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	DATA_RECORD: {
		Token:    DATA_RECORD,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "record_description",
	},
	CODE_SET: {
		Token:    CODE_SET,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_description",
	},
	SHARING: {
		Token:    SHARING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_sharing",
	},
	WITH: {
		Token:    WITH,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_sharing",
	},
	NO: {
		Token:    NO,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_sharing",
	},
	OTHER: {
		Token:    OTHER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_sharing",
	},
	LOCK: {
		Token:    LOCK,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_sharing",
	},
	AUTOMATIC: {
		Token:    AUTOMATIC,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_sharing",
	},
	MANUAL: {
		Token:    MANUAL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_sharing",
	},
	EXCLUSIVE: {
		Token:    EXCLUSIVE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_sharing",
	},
	RETRY: {
		Token:    RETRY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file_sharing",
	},
	REPORT: {
		Token:    REPORT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "report_writer",
	},
	PAGE: {
		Token:    PAGE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "report_writer",
	},
	HEADING: {
		Token:    HEADING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report_writer",
	},
	FIRST: {
		Token:    FIRST,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report_writer",
	},
	LAST: {
		Token:    LAST,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report_writer",
	},
	DETAIL: {
		Token:    DETAIL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report_writer",
	},
	CONTROL: {
		Token:    CONTROL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report_writer",
	},
	FINAL: {
		Token:    FINAL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report_writer",
	},
	SUM: {
		Token:    SUM,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report_writer",
	},
	RESET: {
		Token:    RESET,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report_writer",
	},

	// Add keyword info for additional ISO standard keywords
	SIGN: {
		Token:    SIGN,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "data_description",
	},
	SEPARATE: {
		Token:    SEPARATE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "data_description",
	},
	PACKED_DECIMAL: {
		Token:    PACKED_DECIMAL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "data_description",
	},
	DISPLAY_1: {
		Token:    DISPLAY_1,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "data_description",
	},
	GLOBAL: {
		Token:    GLOBAL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "data_description",
	},
	EXTERNAL: {
		Token:    EXTERNAL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "data_description",
	},
	NUMERIC: {
		Token:    NUMERIC,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "condition",
	},
	ALPHABETIC: {
		Token:    ALPHABETIC,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "condition",
	},
	ALPHABETIC_LOWER: {
		Token:    ALPHABETIC_LOWER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "condition",
	},
	ALPHABETIC_UPPER: {
		Token:    ALPHABETIC_UPPER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "condition",
	},
	CLASS: {
		Token:    CLASS,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "condition",
	},
	POSITIVE: {
		Token:    POSITIVE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "condition",
	},
	NEGATIVE: {
		Token:    NEGATIVE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "condition",
	},
	DECIMAL_POINT: {
		Token:    DECIMAL_POINT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "environment",
	},
	CURRENCY: {
		Token:    CURRENCY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "environment",
	},
	CONSOLE: {
		Token:    CONSOLE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "environment",
	},
	PRINTER: {
		Token:    PRINTER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "environment",
	},
	SYSIN: {
		Token:    SYSIN,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "environment",
	},
	SYSOUT: {
		Token:    SYSOUT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "environment",
	},
	USING: {
		Token:    USING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "procedure",
	},
	RETURNING: {
		Token:    RETURNING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "procedure",
	},
	RAISING: {
		Token:    RAISING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "procedure",
	},
	EXCEPTION: {
		Token:    EXCEPTION,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "condition",
	},
	SIZE: {
		Token:    SIZE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "procedure",
	},
	ERROR: {
		Token:    ERROR,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "procedure",
	},
	OVERFLOW: {
		Token:    OVERFLOW,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "procedure",
	},
	UNDERFLOW: {
		Token:    UNDERFLOW,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "procedure",
	},
	REMAINDER: {
		Token:    REMAINDER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{END_DIVIDE},
		Category: "arithmetic",
	},
	POINTER: {
		Token:    POINTER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "usage",
	},
	INDEX: {
		Token:    INDEX,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "usage",
	},
	NATIONAL: {
		Token:    NATIONAL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "usage",
	},
	INVALID_KEY: {
		Token:    INVALID_KEY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{END_READ, END_WRITE, START, DELETE},
		Category: "condition",
	},
	AT_END: {
		Token:    AT_END,
		Class:    CLASS_KEYWORD,
		Context:  []Token{END_READ},
		Category: "condition",
	},

	// Report Writer tokens
	INITIATE: {
		Token:    INITIATE,
		Class:    CLASS_KEYWORD,
		Category: "report",
	},
	TERMINATE: {
		Token:    TERMINATE,
		Class:    CLASS_KEYWORD,
		Category: "report",
	},
	LINE_COUNTER: {
		Token:    LINE_COUNTER,
		Class:    CLASS_KEYWORD,
		Category: "report",
	},
	PAGE_COUNTER: {
		Token:    PAGE_COUNTER,
		Class:    CLASS_KEYWORD,
		Category: "report",
	},
	NEXT_GROUP: {
		Token:    NEXT_GROUP,
		Class:    CLASS_KEYWORD,
		Category: "report",
	},
	NEXT_PAGE: {
		Token:    NEXT_PAGE,
		Class:    CLASS_KEYWORD,
		Category: "report",
	},
	DE: {
		Token:    DE,
		Class:    CLASS_KEYWORD,
		Category: "report",
	},
	RH: {
		Token:    RH,
		Class:    CLASS_KEYWORD,
		Category: "report",
	},
	PH: {
		Token:    PH,
		Class:    CLASS_KEYWORD,
		Category: "report",
	},
	RF: {
		Token:    RF,
		Class:    CLASS_KEYWORD,
		Category: "report",
	},
	PF: {
		Token:    PF,
		Class:    CLASS_KEYWORD,
		Category: "report",
	},
	CH: {
		Token:    CH,
		Class:    CLASS_KEYWORD,
		Category: "report",
	},
	CF: {
		Token:    CF,
		Class:    CLASS_KEYWORD,
		Category: "report",
	},
	GROUP_INDICATE: {
		Token:    GROUP_INDICATE,
		Class:    CLASS_KEYWORD,
		Category: "report",
	},
	PICTURE: {
		Token:    PICTURE,
		Class:    CLASS_KEYWORD,
		Aliases:  []string{"PIC"},
		Context:  []Token{DATA_DIVISION},
		Category: "data description",
	},
}

// IsKeyword checks if a token is a keyword
func IsKeyword(t Token) bool {
	_, ok := keywordInfos[t]
	return ok
}

// GetKeywordInfo returns information about a keyword
func GetKeywordInfo(t Token) (KeywordInfo, bool) {
	info, ok := keywordInfos[t]
	return info, ok
}

// LookupKeyword looks up a string to see if it's a keyword
func LookupKeyword(s string) (Token, bool) {
	// This will be implemented with a proper lookup map
	// For now just return unknown
	return ILLEGAL, false
}
