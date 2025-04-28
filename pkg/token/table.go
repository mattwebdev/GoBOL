package token

// TableInfo contains information about a COBOL table handling token
type TableInfo struct {
	Token    Token      // The token representing this table element
	Class    TokenClass // The token class
	Context  []Token    // Valid in context of these verbs
	Category string     // Logical category
}

// tableInfos maps tokens to their table handling information
var tableInfos = map[Token]TableInfo{
	OCCURS: {
		Token:    OCCURS,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "table handling",
	},
	DEPENDING_ON: {
		Token:    DEPENDING_ON,
		Class:    CLASS_KEYWORD,
		Context:  []Token{OCCURS},
		Category: "table handling",
	},
	ASCENDING: {
		Token:    ASCENDING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{OCCURS},
		Category: "table handling",
	},
	DESCENDING: {
		Token:    DESCENDING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{OCCURS},
		Category: "table handling",
	},
	INDEXED_BY: {
		Token:    INDEXED_BY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{OCCURS},
		Category: "table handling",
	},
	KEY_IS: {
		Token:    KEY_IS,
		Class:    CLASS_KEYWORD,
		Context:  []Token{OCCURS},
		Category: "table handling",
	},
}

// IsTable returns true if the token represents a table handling token
func IsTable(tok Token) bool {
	_, ok := tableInfos[tok]
	return ok
}

// GetTableInfo returns information about a table handling token
func GetTableInfo(tok Token) (TableInfo, bool) {
	info, ok := tableInfos[tok]
	return info, ok
}
