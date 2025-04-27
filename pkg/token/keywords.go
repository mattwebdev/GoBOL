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
