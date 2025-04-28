package token

// UsageInfo contains information about a COBOL usage token
type UsageInfo struct {
	Token    Token      // The token representing this usage
	Class    TokenClass // The token class
	Context  []Token    // Valid in context of these verbs
	Category string     // Logical category
}

// usageInfos maps tokens to their usage information
var usageInfos = map[Token]UsageInfo{
	USAGE_OBJECT: {
		Token:    USAGE_OBJECT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "usage",
	},
	USAGE_REFERENCE: {
		Token:    USAGE_REFERENCE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "usage",
	},
	USAGE_POINTER_64: {
		Token:    USAGE_POINTER_64,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "usage",
	},
	USAGE_FUNCTION_POINTER: {
		Token:    USAGE_FUNCTION_POINTER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "usage",
	},
	USAGE_PROGRAM_POINTER: {
		Token:    USAGE_PROGRAM_POINTER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "usage",
	},
	USAGE_METHOD_POINTER: {
		Token:    USAGE_METHOD_POINTER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "usage",
	},
	USAGE_INDEX_64: {
		Token:    USAGE_INDEX_64,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "usage",
	},
	USAGE_INDEX_32: {
		Token:    USAGE_INDEX_32,
		Class:    CLASS_KEYWORD,
		Context:  []Token{},
		Category: "usage",
	},
}

// IsUsage returns true if the token represents a usage token
func IsUsage(tok Token) bool {
	_, ok := usageInfos[tok]
	return ok
}

// GetUsageInfo returns information about a usage token
func GetUsageInfo(tok Token) (UsageInfo, bool) {
	info, ok := usageInfos[tok]
	return info, ok
}
