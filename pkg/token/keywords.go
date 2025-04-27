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
