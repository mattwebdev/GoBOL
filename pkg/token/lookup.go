package token

import "strings"

// tokenMap maps string representations to their corresponding tokens
var tokenMap = make(map[string]Token)

// Initialize the token map
func init() {
	// Add verbs
	for verb := range verbPatterns {
		tokenMap[verbPatterns[verb].Token.String()] = verb
	}

	// Add keywords
	for kw := range keywordInfo {
		tokenMap[keywordInfo[kw].Token.String()] = kw
		// Add aliases
		for _, alias := range keywordInfo[kw].Aliases {
			tokenMap[alias] = kw
		}
	}

	// Add operators
	for op := range operatorInfo {
		if operatorInfo[op].Symbol != "" {
			tokenMap[operatorInfo[op].Symbol] = op
		}
		// Add keyword representations
		for _, kw := range operatorInfo[op].Keywords {
			tokenMap[kw] = op
		}
	}

	// Add divisions
	for div := range divisionInfo {
		tokenMap[divisionInfo[div].Name] = div
	}

	// Add figurative constants
	for fc := range figurativeConstants {
		tokenMap[figurativeConstants[fc].Token.String()] = fc
		// Add aliases
		for _, alias := range figurativeConstants[fc].Aliases {
			tokenMap[alias.String()] = fc
		}
	}
}

// LookupToken looks up a string to find its corresponding token
func LookupToken(s string) Token {
	// COBOL is case-insensitive
	s = strings.ToUpper(s)

	// Check for level numbers
	if IsLevelNumber(s) {
		return LEVEL_NUMBER
	}

	// Check for numeric literals
	if IsNumeric(s) {
		return NUMBER_LIT
	}

	// Check for string literals (quoted)
	if IsStringLiteral(s) {
		return STRING_LIT
	}

	// Look up in the token map
	if tok, ok := tokenMap[s]; ok {
		return tok
	}

	// If not found and valid identifier
	if IsValidIdentifier(s) {
		return IDENTIFIER
	}

	return ILLEGAL
}

// IsLevelNumber checks if a string is a valid COBOL level number
func IsLevelNumber(s string) bool {
	// Level numbers are 01-49, 66, 77, or 88
	switch s {
	case "01", "1", "77", "88":
		return true
	default:
		if len(s) == 2 {
			if s[0] == '0' {
				s = s[1:]
			}
			n := 0
			for _, c := range s {
				if c < '0' || c > '9' {
					return false
				}
				n = n*10 + int(c-'0')
			}
			return n > 0 && n < 50
		}
		return false
	}
}

// IsNumeric checks if a string is a valid COBOL numeric literal
func IsNumeric(s string) bool {
	// TODO: Implement proper COBOL numeric literal validation
	// This is a simplified version
	if len(s) == 0 {
		return false
	}

	hasDigit := false
	for i, c := range s {
		if i == 0 && (c == '+' || c == '-') {
			continue
		}
		if c == '.' {
			continue
		}
		if c < '0' || c > '9' {
			return false
		}
		hasDigit = true
	}
	return hasDigit
}

// IsStringLiteral checks if a string is a valid COBOL string literal
func IsStringLiteral(s string) bool {
	if len(s) < 2 {
		return false
	}
	// COBOL allows both single and double quotes
	return (s[0] == '"' && s[len(s)-1] == '"') ||
		(s[0] == '\'' && s[len(s)-1] == '\'')
}

// IsValidIdentifier checks if a string is a valid COBOL identifier
func IsValidIdentifier(s string) bool {
	if len(s) == 0 || len(s) > 30 {
		return false
	}

	// First character must be letter or hyphen
	if !isLetter(rune(s[0])) && s[0] != '-' {
		return false
	}

	// Rest can be letters, numbers, or hyphens
	for _, c := range s[1:] {
		if !isLetter(c) && !isDigit(c) && c != '-' {
			return false
		}
	}

	return true
}

// Helper functions
func isLetter(c rune) bool {
	return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
}

func isDigit(c rune) bool {
	return c >= '0' && c <= '9'
}
