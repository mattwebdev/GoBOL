package token

// ConditionInfo contains information about a COBOL condition
type ConditionInfo struct {
	Token    Token      // The token representing this condition
	Class    TokenClass // The token class (always CLASS_CONDITION)
	Context  []Token    // Valid in context of these verbs
	Category string     // Logical category
}

// conditionInfos maps tokens to their condition information
var conditionInfos = map[Token]ConditionInfo{
	ON_EXCEPTION: {
		Token:    ON_EXCEPTION,
		Class:    CLASS_CONDITION,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "condition",
	},
	NOT_ON_EXCEPTION: {
		Token:    NOT_ON_EXCEPTION,
		Class:    CLASS_CONDITION,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "condition",
	},
	ON_OVERFLOW: {
		Token:    ON_OVERFLOW,
		Class:    CLASS_CONDITION,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "condition",
	},
	NOT_ON_OVERFLOW: {
		Token:    NOT_ON_OVERFLOW,
		Class:    CLASS_CONDITION,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "condition",
	},
	ON_SIZE_ERROR: {
		Token:    ON_SIZE_ERROR,
		Class:    CLASS_CONDITION,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "condition",
	},
	NOT_ON_SIZE_ERROR: {
		Token:    NOT_ON_SIZE_ERROR,
		Class:    CLASS_CONDITION,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "condition",
	},
}

// IsCondition returns true if the token represents a condition
func IsCondition(tok Token) bool {
	_, ok := conditionInfos[tok]
	return ok
}

// GetConditionInfo returns information about a condition token
func GetConditionInfo(tok Token) (ConditionInfo, bool) {
	info, ok := conditionInfos[tok]
	return info, ok
}
