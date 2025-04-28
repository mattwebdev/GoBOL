package token

// FileOrgInfo contains information about a COBOL file organization
type FileOrgInfo struct {
	Token    Token      // The token representing this file organization
	Class    TokenClass // The token class (always CLASS_FILE)
	Context  []Token    // Valid in context of these verbs
	Category string     // Logical category
}

// fileOrgInfos maps tokens to their file organization information
var fileOrgInfos = map[Token]FileOrgInfo{
	LINE_SEQUENTIAL: {
		Token:    LINE_SEQUENTIAL,
		Class:    CLASS_FILE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file organization",
	},
	QUEUE: {
		Token:    QUEUE,
		Class:    CLASS_FILE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file organization",
	},
	RELATIVE_KEY: {
		Token:    RELATIVE_KEY,
		Class:    CLASS_FILE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file organization",
	},
	RECORD_KEY: {
		Token:    RECORD_KEY,
		Class:    CLASS_FILE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file organization",
	},
}

// IsFileOrg returns true if the token represents a file organization
func IsFileOrg(tok Token) bool {
	_, ok := fileOrgInfos[tok]
	return ok
}

// GetFileOrgInfo returns information about a file organization token
func GetFileOrgInfo(tok Token) (FileOrgInfo, bool) {
	info, ok := fileOrgInfos[tok]
	return info, ok
}
