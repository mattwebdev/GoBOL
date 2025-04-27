package token

import (
	"strconv"
	"strings"
)

// tokenMap maps string representations to their corresponding tokens
var tokenMap = make(map[string]Token)

// Initialize the token map
func init() {
	tokenMap = make(map[string]Token)

	// Add verbs
	for verb, info := range verbPatterns {
		// Add the base verb token
		tokenMap[tokenStrings[verb]] = verb
		// Add any pattern parts
		for _, pattern := range info.Patterns {
			for _, part := range pattern.Parts {
				if part != IDENTIFIER && part != STRING_LIT && part != NUMBER_LIT {
					tokenMap[tokenStrings[part]] = part
				}
			}
		}
	}

	// Add keywords
	for kw, info := range keywordInfo {
		tokenMap[tokenStrings[kw]] = kw
		// Add aliases
		for _, alias := range info.Aliases {
			tokenMap[alias] = kw
		}
	}

	// Add operators
	for op, info := range operatorInfo {
		if info.Symbol != "" {
			tokenMap[info.Symbol] = op
		}
		// Add keyword representations
		for _, kw := range info.Keywords {
			tokenMap[kw] = op
		}
	}

	// Add divisions
	for div, info := range divisionInfo {
		tokenMap[info.Name] = div
	}

	// Add figurative constants
	for fc, info := range figurativeConstants {
		// Add the main token
		tokenMap[tokenStrings[fc]] = fc
		// Add hyphenated version if it exists
		if strings.Contains(tokenStrings[fc], "_") {
			hyphenated := strings.ReplaceAll(tokenStrings[fc], "_", "-")
			tokenMap[hyphenated] = fc
		}
		// Add aliases
		for _, alias := range info.Aliases {
			// Add the alias token string
			tokenMap[tokenStrings[alias]] = fc
			// Add hyphenated version of alias if it exists
			if strings.Contains(tokenStrings[alias], "_") {
				hyphenated := strings.ReplaceAll(tokenStrings[alias], "_", "-")
				tokenMap[hyphenated] = fc
			}
		}
	}
}

// LookupToken looks up a string to find its corresponding token
func LookupToken(s string) Token {
	// COBOL is case-insensitive
	s = strings.ToUpper(s)

	// Empty string is illegal
	if s == "" {
		return ILLEGAL
	}

	// Check for string literals (quoted)
	if IsStringLiteral(s) {
		return STRING_LIT
	}

	// Check for level numbers first
	if IsLevelNumber(s) {
		return LEVEL_NUMBER
	}

	// Then check for numeric literals
	if IsNumeric(s) {
		// Check if it's a number between 50-99 (except 66, 77, 88)
		if len(s) <= 2 {
			if n, err := strconv.Atoi(s); err == nil && n >= 50 && n <= 99 {
				if n != 66 && n != 77 && n != 88 {
					return IDENTIFIER
				}
			}
		}
		return NUMBER_LIT
	}

	// Special case: standalone period is illegal
	if s == "." {
		return ILLEGAL
	}

	// Look up in the token map (for keywords, verbs, etc.)
	if tok, ok := tokenMap[s]; ok {
		return tok
	}

	// If not found and valid identifier
	if IsValidIdentifier(s) {
		return IDENTIFIER
	}

	// Handle identifiers with trailing quotes
	if strings.HasSuffix(s, "\"") || strings.HasSuffix(s, "'") {
		base := s[:len(s)-1]
		if IsValidIdentifier(base) {
			return IDENTIFIER
		}
	}

	return ILLEGAL
}

// IsLevelNumber checks if a string is a valid COBOL level number
func IsLevelNumber(s string) bool {
	// Special level numbers
	switch s {
	case "66", "77", "88":
		return true
	}

	// Level numbers can't have + or - prefix
	if strings.HasPrefix(s, "+") || strings.HasPrefix(s, "-") {
		return false
	}

	// Must be all digits
	for _, c := range s {
		if !isDigit(c) {
			return false
		}
	}

	// Convert to number
	n, err := strconv.Atoi(s)
	if err != nil {
		return false
	}

	// Regular level numbers are 1-49 (with or without leading zero)
	if n >= 1 && n <= 49 {
		// But numbers like 42 that could be numeric literals should not be level numbers
		if n >= 10 && n != 49 {
			return false
		}
		return true
	}

	return false
}

// IsNumeric checks if a string is a valid numeric literal
func IsNumeric(s string) bool {
	if len(s) == 0 {
		return false
	}

	// Handle sign prefix
	start := 0
	if s[0] == '+' || s[0] == '-' {
		if len(s) == 1 {
			return false
		}
		start = 1
	}

	// Must have at least one digit
	hasDigit := false
	foundDecimal := false

	for i := start; i < len(s); i++ {
		if s[i] == '.' {
			if foundDecimal {
				return false // Multiple decimal points
			}
			foundDecimal = true
		} else if isDigit(rune(s[i])) {
			hasDigit = true
		} else {
			// If we find a non-digit, non-decimal character, it might be a variable name
			// starting with a number (like "1VAR")
			return hasDigit
		}
	}

	// Special cases
	if s == "00" {
		return false // "00" is ILLEGAL in COBOL
	}

	return hasDigit // Must have at least one digit
}

// IsStringLiteral checks if a string is a valid COBOL string literal
func IsStringLiteral(s string) bool {
	if len(s) < 2 {
		return false
	}

	// Check for proper quotes
	firstChar := s[0]
	lastChar := s[len(s)-1]

	// Must start with a quote
	if firstChar != '"' && firstChar != '\'' {
		return false
	}

	// Must end with matching quote
	if lastChar != firstChar {
		return false
	}

	// Check for balanced quotes
	return true
}

// IsValidIdentifier checks if a string is a valid COBOL identifier
func IsValidIdentifier(s string) bool {
	if len(s) == 0 || len(s) > 30 {
		return false
	}

	// Must start with a letter or hyphen
	if !isLetter(rune(s[0])) && s[0] != '-' {
		return false
	}

	// If starts with hyphen, second character must be a letter
	if s[0] == '-' {
		if len(s) < 2 || !isLetter(rune(s[1])) {
			return false
		}
	}

	// Check remaining characters
	hasNonHyphen := false
	for _, c := range s {
		if !isLetter(c) && !isDigit(c) && c != '-' {
			return false
		}
		if c != '-' {
			hasNonHyphen = true
		}
	}

	// Must contain at least one non-hyphen character
	return hasNonHyphen
}

// Helper functions
func isLetter(c rune) bool {
	return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
}

func isDigit(c rune) bool {
	return c >= '0' && c <= '9'
}
