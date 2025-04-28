package token

// ScopeInfo contains information about a COBOL scope terminator
type ScopeInfo struct {
	Token    Token      // The token representing this scope terminator
	Class    TokenClass // The token class (always CLASS_SCOPE)
	Context  []Token    // Valid in context of these verbs
	Category string     // Logical category
}

// scopeInfos maps tokens to their scope information
var scopeInfos = map[Token]ScopeInfo{
	END_START: {
		Token:    END_START,
		Class:    CLASS_SCOPE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope terminator",
	},
	END_DELETE: {
		Token:    END_DELETE,
		Class:    CLASS_SCOPE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope terminator",
	},
	END_REWRITE: {
		Token:    END_REWRITE,
		Class:    CLASS_SCOPE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope terminator",
	},
	END_RETURN: {
		Token:    END_RETURN,
		Class:    CLASS_SCOPE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope terminator",
	},
	END_INITIALIZE: {
		Token:    END_INITIALIZE,
		Class:    CLASS_SCOPE,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "scope terminator",
	},
}

// IsScope returns true if the token represents a scope terminator
func IsScope(tok Token) bool {
	_, ok := scopeInfos[tok]
	return ok
}

// GetScopeInfo returns information about a scope terminator token
func GetScopeInfo(tok Token) (ScopeInfo, bool) {
	info, ok := scopeInfos[tok]
	return info, ok
}
