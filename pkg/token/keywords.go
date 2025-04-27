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
var keywordInfo = map[Token]KeywordInfo{
	CORRESPONDING: {
		Token:    CORRESPONDING,
		Class:    CLASS_MODIFIER,
		Aliases:  []string{"CORR"},
		Context:  []Token{MOVE, ADD, SUBTRACT},
		Category: "modifier",
	},
	TO: {
		Token:    TO,
		Class:    CLASS_KEYWORD,
		Context:  []Token{MOVE, ADD, SUBTRACT},
		Category: "destination",
	},
	GIVING: {
		Token:    GIVING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{ADD, SUBTRACT, MULTIPLY, DIVIDE},
		Category: "destination",
	},
	GREATER: {
		Token:    GREATER,
		Class:    CLASS_KEYWORD,
		Category: "comparison",
	},
	THAN: {
		Token:    THAN,
		Class:    CLASS_KEYWORD,
		Context:  []Token{GREATER, LESS},
		Category: "comparison",
	},
	COMPUTATIONAL: {
		Token:    COMPUTATIONAL,
		Class:    CLASS_KEYWORD,
		Aliases:  []string{"COMP"},
		Category: "usage",
	},
	COMP: {
		Token:    COMP,
		Class:    CLASS_KEYWORD,
		Category: "usage",
	},
	COMP_1: {
		Token:    COMP_1,
		Class:    CLASS_KEYWORD,
		Category: "usage",
	},
	COMP_2: {
		Token:    COMP_2,
		Class:    CLASS_KEYWORD,
		Category: "usage",
	},
	COMP_3: {
		Token:    COMP_3,
		Class:    CLASS_KEYWORD,
		Category: "usage",
	},
	COMP_4: {
		Token:    COMP_4,
		Class:    CLASS_KEYWORD,
		Category: "usage",
	},
	COMP_5: {
		Token:    COMP_5,
		Class:    CLASS_KEYWORD,
		Category: "usage",
	},
	BINARY: {
		Token:    BINARY,
		Class:    CLASS_KEYWORD,
		Category: "usage",
	},
	SYNCHRONIZED: {
		Token:    SYNCHRONIZED,
		Class:    CLASS_KEYWORD,
		Aliases:  []string{"SYNC"},
		Category: "alignment",
	},
	SYNC: {
		Token:    SYNC,
		Class:    CLASS_KEYWORD,
		Category: "alignment",
	},
	JUSTIFIED: {
		Token:    JUSTIFIED,
		Class:    CLASS_KEYWORD,
		Aliases:  []string{"JUST"},
		Category: "alignment",
	},
	JUST: {
		Token:    JUST,
		Class:    CLASS_KEYWORD,
		Category: "alignment",
	},
	BLANK: {
		Token:    BLANK,
		Class:    CLASS_KEYWORD,
		Category: "data_description",
	},
	RENAMES: {
		Token:    RENAMES,
		Class:    CLASS_KEYWORD,
		Category: "data_description",
	},
	REDEFINES: {
		Token:    REDEFINES,
		Class:    CLASS_KEYWORD,
		Category: "data_description",
	},
	FILLER: {
		Token:    FILLER,
		Class:    CLASS_KEYWORD,
		Category: "data_description",
	},
	THROUGH: {
		Token:    THROUGH,
		Class:    CLASS_KEYWORD,
		Aliases:  []string{"THRU"},
		Category: "range",
	},
	THRU: {
		Token:    THRU,
		Class:    CLASS_KEYWORD,
		Category: "range",
	},
	WHEN: {
		Token:    WHEN,
		Class:    CLASS_KEYWORD,
		Context:  []Token{EVALUATE},
		Category: "control",
	},
	AFTER: {
		Token:    AFTER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{USE},
		Category: "position",
	},
	BEFORE: {
		Token:    BEFORE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{USE},
		Category: "position",
	},
	STANDARD: {
		Token:    STANDARD,
		Class:    CLASS_KEYWORD,
		Context:  []Token{USE},
		Category: "qualifier",
	},
	BASED: {
		Token:    BASED,
		Class:    CLASS_KEYWORD,
		Context:  []Token{ALLOCATE},
		Category: "memory",
	},
	END_IF: {
		Token:    END_IF,
		Class:    CLASS_KEYWORD,
		Context:  []Token{IF},
		Category: "scope_terminator",
	},
	END_READ: {
		Token:    END_READ,
		Class:    CLASS_KEYWORD,
		Context:  []Token{READ},
		Category: "scope_terminator",
	},
	END_WRITE: {
		Token:    END_WRITE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{WRITE},
		Category: "scope_terminator",
	},
	END_PERFORM: {
		Token:    END_PERFORM,
		Class:    CLASS_KEYWORD,
		Context:  []Token{PERFORM},
		Category: "scope_terminator",
	},
	END_EVALUATE: {
		Token:    END_EVALUATE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{EVALUATE},
		Category: "scope_terminator",
	},
	END_SEARCH: {
		Token:    END_SEARCH,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SEARCH},
		Category: "scope_terminator",
	},
	END_COMPUTE: {
		Token:    END_COMPUTE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{COMPUTE},
		Category: "scope_terminator",
	},
	END_ADD: {
		Token:    END_ADD,
		Class:    CLASS_KEYWORD,
		Context:  []Token{ADD},
		Category: "scope_terminator",
	},
	END_SUBTRACT: {
		Token:    END_SUBTRACT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SUBTRACT},
		Category: "scope_terminator",
	},
	END_MULTIPLY: {
		Token:    END_MULTIPLY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{MULTIPLY},
		Category: "scope_terminator",
	},
	END_DIVIDE: {
		Token:    END_DIVIDE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{DIVIDE},
		Category: "scope_terminator",
	},
	END_STRING: {
		Token:    END_STRING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{STRING_VERB},
		Category: "scope_terminator",
	},
	END_UNSTRING: {
		Token:    END_UNSTRING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{UNSTRING},
		Category: "scope_terminator",
	},
	END_CALL: {
		Token:    END_CALL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{CALL},
		Category: "scope_terminator",
	},
	END_ACCEPT: {
		Token:    END_ACCEPT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{ACCEPT},
		Category: "scope_terminator",
	},
	END_DISPLAY: {
		Token:    END_DISPLAY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{DISPLAY},
		Category: "scope_terminator",
	},

	// File-related keyword info
	ORGANIZATION: {
		Token:    ORGANIZATION,
		Class:    CLASS_KEYWORD,
		Context:  []Token{FILE},
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
		Context:  []Token{FILE},
		Category: "file_description",
	},
	RANDOM: {
		Token:    RANDOM,
		Class:    CLASS_KEYWORD,
		Context:  []Token{ACCESS},
		Category: "access_mode",
	},
	DYNAMIC: {
		Token:    DYNAMIC,
		Class:    CLASS_KEYWORD,
		Context:  []Token{ACCESS},
		Category: "access_mode",
	},
	FILE: {
		Token:    FILE,
		Class:    CLASS_KEYWORD,
		Category: "file",
	},
	STATUS: {
		Token:    STATUS,
		Class:    CLASS_KEYWORD,
		Context:  []Token{FILE},
		Category: "file_description",
	},
	RECORD: {
		Token:    RECORD,
		Class:    CLASS_KEYWORD,
		Category: "record",
	},
	KEY: {
		Token:    KEY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{RECORD},
		Category: "record_description",
	},
	ALTERNATE: {
		Token:    ALTERNATE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{KEY},
		Category: "record_description",
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
		Category: "file_description",
	},
	// Add more keyword patterns as needed...
}

// IsKeyword checks if a token is a keyword
func IsKeyword(t Token) bool {
	_, ok := keywordInfo[t]
	return ok
}

// GetKeywordInfo returns information about a keyword
func GetKeywordInfo(t Token) (KeywordInfo, bool) {
	info, ok := keywordInfo[t]
	return info, ok
}

// LookupKeyword looks up a string to see if it's a keyword
func LookupKeyword(s string) (Token, bool) {
	// This will be implemented with a proper lookup map
	// For now just return unknown
	return ILLEGAL, false
}
