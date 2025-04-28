package token

// TableInfo contains information about a COBOL table token
type TableInfo struct {
	Token    Token      // The token representing this table element
	Class    TokenClass // The token class
	Context  []Token    // Valid in context of these verbs
	Category string     // Logical category
}

// tableInfos maps tokens to their table information
var tableInfos = map[Token]TableInfo{
	OCCURS: {
		Token:    OCCURS,
		Class:    CLASS_KEYWORD,
		Context:  []Token{DATA_DIVISION},
		Category: "table occurs",
	},
	TABLE_OCCURS: {
		Token:    TABLE_OCCURS,
		Class:    CLASS_KEYWORD,
		Context:  []Token{DATA_DIVISION},
		Category: "table occurs",
	},
	TABLE_INDEX: {
		Token:    TABLE_INDEX,
		Class:    CLASS_KEYWORD,
		Context:  []Token{DATA_DIVISION},
		Category: "table index",
	},
	TABLE_KEY: {
		Token:    TABLE_KEY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{DATA_DIVISION},
		Category: "table key",
	},
	TABLE_SEARCH: {
		Token:    TABLE_SEARCH,
		Class:    CLASS_KEYWORD,
		Context:  []Token{PROCEDURE_DIVISION},
		Category: "table search",
	},
	TABLE_SORT: {
		Token:    TABLE_SORT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{PROCEDURE_DIVISION},
		Category: "table sort",
	},
	TABLE_MERGE: {
		Token:    TABLE_MERGE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{PROCEDURE_DIVISION},
		Category: "table merge",
	},
	TABLE_BINARY_SEARCH: {
		Token:    TABLE_BINARY_SEARCH,
		Class:    CLASS_KEYWORD,
		Context:  []Token{PROCEDURE_DIVISION},
		Category: "table binary search",
	},
	TABLE_SEQUENTIAL_SEARCH: {
		Token:    TABLE_SEQUENTIAL_SEARCH,
		Class:    CLASS_KEYWORD,
		Context:  []Token{PROCEDURE_DIVISION},
		Category: "table sequential search",
	},
	DEPENDING_ON: {
		Token:    DEPENDING_ON,
		Class:    CLASS_KEYWORD,
		Context:  []Token{DATA_DIVISION},
		Category: "table depending on",
	},
	ASCENDING: {
		Token:    ASCENDING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{DATA_DIVISION},
		Category: "table ascending",
	},
	DESCENDING: {
		Token:    DESCENDING,
		Class:    CLASS_KEYWORD,
		Context:  []Token{DATA_DIVISION},
		Category: "table descending",
	},
	INDEXED_BY: {
		Token:    INDEXED_BY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{DATA_DIVISION},
		Category: "table indexed by",
	},
	KEY_IS: {
		Token:    KEY_IS,
		Class:    CLASS_KEYWORD,
		Context:  []Token{DATA_DIVISION},
		Category: "table key is",
	},
}

// IsTable checks if a token represents a table token
func IsTable(t Token) bool {
	_, exists := tableInfos[t]
	return exists
}

// GetTableInfo returns information about a table token
func GetTableInfo(t Token) (TableInfo, bool) {
	info, exists := tableInfos[t]
	return info, exists
}
