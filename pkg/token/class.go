package token

// ClassInfo contains information about a COBOL class token
type ClassInfo struct {
	Token    Token      // The token representing this class
	Class    TokenClass // The token class
	Context  []Token    // Valid in context of these verbs
	Category string     // Logical category
}

// classInfos maps tokens to their class information
var classInfos = map[Token]ClassInfo{
	CLASS_ALPHABETIC: {
		Token:    CLASS_ALPHABETIC,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "class condition",
	},
	CLASS_ALPHANUMERIC: {
		Token:    CLASS_ALPHANUMERIC,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "class condition",
	},
	CLASS_NUMERIC: {
		Token:    CLASS_NUMERIC,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "class condition",
	},
	CLASS_BOOLEAN: {
		Token:    CLASS_BOOLEAN,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "class condition",
	},
	CLASS_OBJECT: {
		Token:    CLASS_OBJECT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "class condition",
	},
	CLASS_REFERENCE: {
		Token:    CLASS_REFERENCE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "class condition",
	},
	CLASS_OBJECT_REFERENCE: {
		Token:    CLASS_OBJECT_REFERENCE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "class condition",
	},
	CLASS_ALPHANUMERIC_EDITED: {
		Token:    CLASS_ALPHANUMERIC_EDITED,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "class condition",
	},
	CLASS_NUMERIC_EDITED: {
		Token:    CLASS_NUMERIC_EDITED,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "class condition",
	},
	CLASS_DBCS: {
		Token:    CLASS_DBCS,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "class condition",
	},
	CLASS_KANJI: {
		Token:    CLASS_KANJI,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "class condition",
	},
	CLASS_NATIONAL_EDITED: {
		Token:    CLASS_NATIONAL_EDITED,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "class condition",
	},
	CLASS_BOOLEAN_OBJECT: {
		Token:    CLASS_BOOLEAN_OBJECT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "class condition",
	},
	CLASS_BOOLEAN_REFERENCE: {
		Token:    CLASS_BOOLEAN_REFERENCE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "class condition",
	},
}

// IsClass returns true if the token represents a class token
func IsClass(tok Token) bool {
	_, ok := classInfos[tok]
	return ok
}

// GetClassInfo returns information about a class token
func GetClassInfo(tok Token) (ClassInfo, bool) {
	info, ok := classInfos[tok]
	return info, ok
}
