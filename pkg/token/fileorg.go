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
	FILE_SHARING: {
		Token:    FILE_SHARING,
		Class:    CLASS_FILE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file sharing",
	},
	FILE_LOCKING: {
		Token:    FILE_LOCKING,
		Class:    CLASS_FILE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file locking",
	},
	FILE_RECORDING: {
		Token:    FILE_RECORDING,
		Class:    CLASS_FILE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file recording",
	},
	FILE_ACCESS_MODE: {
		Token:    FILE_ACCESS_MODE,
		Class:    CLASS_FILE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file access mode",
	},
	FILE_ORGANIZATION_INDEXED: {
		Token:    FILE_ORGANIZATION_INDEXED,
		Class:    CLASS_FILE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file organization",
	},
	FILE_ORGANIZATION_RELATIVE: {
		Token:    FILE_ORGANIZATION_RELATIVE,
		Class:    CLASS_FILE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "file organization",
	},
	FILE_ORGANIZATION_SEQUENTIAL: {
		Token:    FILE_ORGANIZATION_SEQUENTIAL,
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
