package token

import (
	"fmt"
)

// ValidationError represents an error in token validation
type ValidationError struct {
	Message string
	Token   Token
	Context []Token
}

func (e *ValidationError) Error() string {
	return e.Message
}

// ValidateTokenContext checks if a token is valid in its current context
func ValidateTokenContext(t Token, context []Token) error {
	// Check if token is a verb
	if IsVerb(t) {
		info, _ := GetVerbInfo(t)
		if !isValidInDivision(info, context) {
			return fmt.Errorf("verb not allowed in current division")
		}
	}

	// Check if token is a keyword
	if IsKeyword(t) {
		// Keywords are generally not allowed in PROCEDURE_DIVISION
		for _, ctx := range context {
			if ctx == PROCEDURE_DIVISION {
				return fmt.Errorf("keyword not allowed in procedure division")
			}
		}
		info, _ := GetKeywordInfo(t)
		if !isValidKeywordContext(info, context) {
			return fmt.Errorf("keyword not allowed in current context")
		}
	}

	// Check if token is a condition
	if IsCondition(t) {
		info, _ := GetConditionInfo(t)
		if !isValidConditionContext(info, context) {
			return fmt.Errorf("condition not allowed in current context")
		}
	}

	// Check if token is a file handling token
	if IsFileOrg(t) {
		info, _ := GetFileOrgInfo(t)
		if !isValidFileOrgContext(info, context) {
			return fmt.Errorf("file handling token not allowed in current context")
		}
	}

	// Check if token is a report writer token
	if IsReport(t) {
		info, _ := GetReportInfo(t)
		if !isValidReportContext(info, context) {
			return fmt.Errorf("report writer token not allowed in current context")
		}
	}

	return nil
}

// ValidateTokenGroup checks if a group of tokens forms a valid statement
func ValidateTokenGroup(tokens []Token, context []Token) error {
	if len(tokens) == 0 {
		return fmt.Errorf("empty token group")
	}

	// Check if the first token is a verb
	if !IsVerb(tokens[0]) {
		return fmt.Errorf("statement must start with a verb")
	}

	// Validate the statement based on the verb
	switch tokens[0] {
	case MOVE:
		return validateMoveStatement(tokens[1:])
	case IF:
		return validateIfStatement(tokens[1:])
	default:
		return nil
	}
}

// Helper functions
func isValidInDivision(info VerbInfo, context []Token) bool {
	// Check if we're in a valid division for this verb
	for _, tok := range context {
		switch tok {
		case PROCEDURE_DIVISION:
			return true
		case IDENTIFICATION_DIVISION, ENVIRONMENT_DIVISION, DATA_DIVISION:
			return false
		}
	}
	return false
}

func isValidInContext(t Token, context []Token) bool {
	// Check if token is a keyword
	if IsKeyword(t) {
		info, _ := GetKeywordInfo(t)
		// Keywords are generally not allowed in PROCEDURE_DIVISION
		for _, ctx := range context {
			if ctx == PROCEDURE_DIVISION {
				return false
			}
		}
		return isValidKeywordContext(info, context)
	}

	// Check if token is a condition
	if IsCondition(t) {
		info, _ := GetConditionInfo(t)
		return isValidConditionContext(info, context)
	}

	// Check if token is a file handling token
	if IsFileOrg(t) {
		info, _ := GetFileOrgInfo(t)
		return isValidFileOrgContext(info, context)
	}

	// Check if token is a report writer token
	if IsReport(t) {
		info, _ := GetReportInfo(t)
		return isValidReportContext(info, context)
	}

	return true
}

// isValidKeywordContext checks if a keyword is valid in its current context
func isValidKeywordContext(info KeywordInfo, context []Token) bool {
	// Check if keyword is allowed in current context
	for _, ctx := range info.Context {
		for _, tok := range context {
			if tok == ctx {
				return true
			}
		}
	}
	return false
}

// isValidConditionContext checks if a condition is valid in its current context
func isValidConditionContext(info ConditionInfo, context []Token) bool {
	// Check if condition is allowed in current context
	for _, ctx := range info.Context {
		for _, tok := range context {
			if tok == ctx {
				return true
			}
		}
	}
	return false
}

// isValidFileOrgContext checks if a file organization token is valid in its current context
func isValidFileOrgContext(info FileOrgInfo, context []Token) bool {
	// Check if the token is in its allowed context
	for _, allowed := range info.Context {
		for _, ctx := range context {
			if ctx == allowed {
				return true
			}
		}
	}
	return false
}

// isValidReportContext checks if a report writer token is valid in its current context
func isValidReportContext(info ReportInfo, context []Token) bool {
	// Check if the token is valid in any of its allowed contexts
	for _, allowedContext := range info.Context {
		for _, ctx := range context {
			if ctx == allowedContext {
				return true
			}
		}
	}
	return false
}

// validateMoveStatement checks if a MOVE statement is valid
func validateMoveStatement(tokens []Token) error {
	if len(tokens) < 3 {
		return fmt.Errorf("MOVE statement requires at least two operands and TO")
	}
	hasTo := false
	for _, t := range tokens {
		if t == TO {
			hasTo = true
			break
		}
	}
	if !hasTo {
		return fmt.Errorf("MOVE statement requires TO keyword")
	}
	return nil
}

// validateIfStatement checks if an IF statement is valid
func validateIfStatement(tokens []Token) error {
	if len(tokens) < 2 {
		return fmt.Errorf("IF statement requires a condition and a THEN clause")
	}
	hasThen := false
	for _, t := range tokens {
		if t == THEN {
			hasThen = true
			break
		}
	}
	if !hasThen {
		return fmt.Errorf("IF statement requires THEN keyword")
	}
	return nil
}
