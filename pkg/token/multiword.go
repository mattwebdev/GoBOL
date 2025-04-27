package token

// MultiWordPattern represents a pattern for multi-word tokens
type MultiWordPattern struct {
	Parts    []Token // The sequence of tokens that make up this pattern
	Result   Token   // The resulting token when this pattern is matched
	Optional []bool  // Which parts are optional (e.g., "CORRESPONDING" in "MOVE CORRESPONDING")
	Context  []Token // Valid only in these contexts (empty means valid in any context)
}

// Common multi-word patterns in COBOL
var multiWordPatterns = []MultiWordPattern{
	// MOVE patterns
	{
		Parts:    []Token{MOVE, CORRESPONDING},
		Result:   MOVE,
		Optional: []bool{false, false},
	},
	{
		Parts:    []Token{MOVE, CORR}, // CORR is alias for CORRESPONDING
		Result:   MOVE,
		Optional: []bool{false, false},
	},

	// Comparison patterns
	{
		Parts:    []Token{GREATER, THAN},
		Result:   OP_GREATER_THAN,
		Optional: []bool{false, false},
	},
	{
		Parts:    []Token{LESS, THAN},
		Result:   OP_LESS_THAN,
		Optional: []bool{false, false},
	},
	{
		Parts:    []Token{EQUAL, TO},
		Result:   OP_EQUAL_TO,
		Optional: []bool{false, true}, // "TO" is optional
	},
	{
		Parts:    []Token{GREATER, THAN, OR, EQUAL, TO},
		Result:   OP_GREATER_EQUAL,
		Optional: []bool{false, false, false, false, true}, // "TO" is optional
	},

	// GO TO pattern
	{
		Parts:    []Token{GO, TO},
		Result:   GO_TO,
		Optional: []bool{false, false},
	},

	// Division patterns
	{
		Parts:    []Token{IDENTIFICATION, DIVISION},
		Result:   IDENTIFICATION_DIVISION,
		Optional: []bool{false, false},
	},
	{
		Parts:    []Token{ENVIRONMENT, DIVISION},
		Result:   ENVIRONMENT_DIVISION,
		Optional: []bool{false, false},
	},
	{
		Parts:    []Token{DATA, DIVISION},
		Result:   DATA_DIVISION,
		Optional: []bool{false, false},
	},
	{
		Parts:    []Token{PROCEDURE, DIVISION},
		Result:   PROCEDURE_DIVISION,
		Optional: []bool{false, false},
	},
}

// MultiWordLookup contains precomputed information for quick multi-word token lookup
type MultiWordLookup struct {
	FirstWord Token
	Patterns  []MultiWordPattern
	MaxLength int
}

// Quick lookup map for multi-word patterns based on their first word
var multiWordLookup map[Token]*MultiWordLookup

func init() {
	// Initialize the quick lookup map
	multiWordLookup = make(map[Token]*MultiWordLookup)

	// Group patterns by their first word for quick lookup
	for _, pattern := range multiWordPatterns {
		if len(pattern.Parts) == 0 {
			continue
		}

		firstWord := pattern.Parts[0]
		lookup, exists := multiWordLookup[firstWord]
		if !exists {
			lookup = &MultiWordLookup{
				FirstWord: firstWord,
				Patterns:  make([]MultiWordPattern, 0),
				MaxLength: 0,
			}
			multiWordLookup[firstWord] = lookup
		}

		lookup.Patterns = append(lookup.Patterns, pattern)
		if len(pattern.Parts) > lookup.MaxLength {
			lookup.MaxLength = len(pattern.Parts)
		}
	}
}

// IsMultiWordStart checks if a token could be the start of a multi-word token
func IsMultiWordStart(t Token) bool {
	_, ok := multiWordLookup[t]
	return ok
}

// GetMultiWordPatterns returns all possible patterns that start with the given token
func GetMultiWordPatterns(t Token) []MultiWordPattern {
	if lookup, ok := multiWordLookup[t]; ok {
		return lookup.Patterns
	}
	return nil
}

// GetMaxPatternLength returns the maximum length of any pattern starting with the given token
func GetMaxPatternLength(t Token) int {
	if lookup, ok := multiWordLookup[t]; ok {
		return lookup.MaxLength
	}
	return 0
}

// MatchMultiWordPattern tries to match a sequence of tokens against known patterns
// Returns the resulting token and number of tokens consumed if a match is found
func MatchMultiWordPattern(tokens []TokenInfo) (Token, int) {
	if len(tokens) == 0 {
		return ILLEGAL, 0
	}

	// Get patterns that could match based on first token
	firstToken := tokens[0].Type
	lookup, ok := multiWordLookup[firstToken]
	if !ok {
		return ILLEGAL, 0
	}

	// Try each pattern
	for _, pattern := range lookup.Patterns {
		matched, consumed := tryMatchPattern(tokens, pattern)
		if matched {
			return pattern.Result, consumed
		}
	}

	return ILLEGAL, 0
}

// tryMatchPattern attempts to match a specific pattern against a sequence of tokens
func tryMatchPattern(tokens []TokenInfo, pattern MultiWordPattern) (bool, int) {
	if len(tokens) < 1 {
		return false, 0
	}

	patternIdx := 0
	tokenIdx := 0

	for patternIdx < len(pattern.Parts) && tokenIdx < len(tokens) {
		if pattern.Optional[patternIdx] {
			// If this part is optional, try matching with and without it
			if tokens[tokenIdx].Type == pattern.Parts[patternIdx] {
				patternIdx++
				tokenIdx++
			} else {
				patternIdx++
				continue
			}
		} else {
			// This part is required
			if tokens[tokenIdx].Type != pattern.Parts[patternIdx] {
				return false, 0
			}
			patternIdx++
			tokenIdx++
		}
	}

	// Check if we matched all required parts
	for i := patternIdx; i < len(pattern.Parts); i++ {
		if !pattern.Optional[i] {
			return false, 0
		}
	}

	return true, tokenIdx
}

// IsValidInContext checks if a multi-word token is valid in the current context
func IsValidInContext(pattern MultiWordPattern, context Token) bool {
	if len(pattern.Context) == 0 {
		return true
	}
	for _, validContext := range pattern.Context {
		if validContext == context {
			return true
		}
	}
	return false
}
