package token

import (
	"testing"
)

func TestIsMultiWordStart(t *testing.T) {
	tests := []struct {
		token    Token
		expected bool
	}{
		{MOVE, true},
		{GREATER, true},
		{LESS, true},
		{GO, true},
		{IDENTIFICATION, true},
		{ENVIRONMENT, true},
		{DATA, true},
		{PROCEDURE, true},
		{ADD, false},
		{ILLEGAL, false},
		{EOF, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := IsMultiWordStart(tt.token); got != tt.expected {
				t.Errorf("IsMultiWordStart(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestGetMultiWordPatterns(t *testing.T) {
	tests := []struct {
		token            Token
		expectedLength   int
		expectedResult   Token
		expectedParts    []Token
		expectedOptional []bool
	}{
		{
			token:            MOVE,
			expectedLength:   2,
			expectedResult:   MOVE,
			expectedParts:    []Token{MOVE, CORRESPONDING},
			expectedOptional: []bool{false, false},
		},
		{
			token:            GREATER,
			expectedLength:   5,
			expectedResult:   OP_GREATER_EQUAL,
			expectedParts:    []Token{GREATER, THAN, OR, EQUAL, TO},
			expectedOptional: []bool{false, false, false, false, true},
		},
		{
			token:          ILLEGAL,
			expectedLength: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			patterns := GetMultiWordPatterns(tt.token)
			if tt.expectedLength == 0 {
				if patterns != nil {
					t.Errorf("GetMultiWordPatterns(%v) = %v, want nil", tt.token, patterns)
				}
				return
			}
			if len(patterns) != tt.expectedLength {
				t.Errorf("GetMultiWordPatterns(%v) length = %v, want %v", tt.token, len(patterns), tt.expectedLength)
				return
			}
			// Test first pattern in detail
			pattern := patterns[0]
			if pattern.Result != tt.expectedResult {
				t.Errorf("Pattern.Result = %v, want %v", pattern.Result, tt.expectedResult)
			}
			if len(pattern.Parts) != len(tt.expectedParts) {
				t.Errorf("Pattern.Parts length = %v, want %v", len(pattern.Parts), len(tt.expectedParts))
			} else {
				for i, part := range tt.expectedParts {
					if pattern.Parts[i] != part {
						t.Errorf("Pattern.Parts[%d] = %v, want %v", i, pattern.Parts[i], part)
					}
				}
			}
			if len(pattern.Optional) != len(tt.expectedOptional) {
				t.Errorf("Pattern.Optional length = %v, want %v", len(pattern.Optional), len(tt.expectedOptional))
			} else {
				for i, opt := range tt.expectedOptional {
					if pattern.Optional[i] != opt {
						t.Errorf("Pattern.Optional[%d] = %v, want %v", i, pattern.Optional[i], opt)
					}
				}
			}
		})
	}
}

func TestMatchMultiWordPattern(t *testing.T) {
	tests := []struct {
		name             string
		tokens           []TokenInfo
		expectedToken    Token
		expectedConsumed int
	}{
		{
			name: "MOVE CORRESPONDING",
			tokens: []TokenInfo{
				{Type: MOVE},
				{Type: CORRESPONDING},
			},
			expectedToken:    MOVE,
			expectedConsumed: 2,
		},
		{
			name: "GREATER THAN OR EQUAL TO",
			tokens: []TokenInfo{
				{Type: GREATER},
				{Type: THAN},
				{Type: OR},
				{Type: EQUAL},
				{Type: TO},
			},
			expectedToken:    OP_GREATER_EQUAL,
			expectedConsumed: 5,
		},
		{
			name: "EQUAL TO (with optional TO)",
			tokens: []TokenInfo{
				{Type: EQUAL},
				{Type: TO},
			},
			expectedToken:    OP_EQUAL_TO,
			expectedConsumed: 2,
		},
		{
			name: "EQUAL (without optional TO)",
			tokens: []TokenInfo{
				{Type: EQUAL},
			},
			expectedToken:    OP_EQUAL_TO,
			expectedConsumed: 1,
		},
		{
			name: "No match",
			tokens: []TokenInfo{
				{Type: ADD},
				{Type: TO},
			},
			expectedToken:    ILLEGAL,
			expectedConsumed: 0,
		},
		{
			name:             "Empty tokens",
			tokens:           []TokenInfo{},
			expectedToken:    ILLEGAL,
			expectedConsumed: 0,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			token, consumed := MatchMultiWordPattern(tt.tokens)
			if token != tt.expectedToken {
				t.Errorf("MatchMultiWordPattern() token = %v, want %v", token, tt.expectedToken)
			}
			if consumed != tt.expectedConsumed {
				t.Errorf("MatchMultiWordPattern() consumed = %v, want %v", consumed, tt.expectedConsumed)
			}
		})
	}
}

func TestGetMaxPatternLength(t *testing.T) {
	tests := []struct {
		token    Token
		expected int
	}{
		{MOVE, 2},           // MOVE CORRESPONDING
		{GREATER, 5},        // GREATER THAN OR EQUAL TO
		{IDENTIFICATION, 2}, // IDENTIFICATION DIVISION
		{ADD, 0},            // Not a multi-word start
		{ILLEGAL, 0},        // Not a multi-word start
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := GetMaxPatternLength(tt.token); got != tt.expected {
				t.Errorf("GetMaxPatternLength(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestIsValidInContext(t *testing.T) {
	tests := []struct {
		name     string
		pattern  MultiWordPattern
		context  Token
		expected bool
	}{
		{
			name: "No context restrictions",
			pattern: MultiWordPattern{
				Parts:    []Token{MOVE, CORRESPONDING},
				Result:   MOVE,
				Optional: []bool{false, false},
				Context:  []Token{},
			},
			context:  ADD,
			expected: true,
		},
		{
			name: "Valid context",
			pattern: MultiWordPattern{
				Parts:    []Token{GREATER, THAN},
				Result:   OP_GREATER_THAN,
				Optional: []bool{false, false},
				Context:  []Token{IF, EVALUATE},
			},
			context:  IF,
			expected: true,
		},
		{
			name: "Invalid context",
			pattern: MultiWordPattern{
				Parts:    []Token{GREATER, THAN},
				Result:   OP_GREATER_THAN,
				Optional: []bool{false, false},
				Context:  []Token{IF, EVALUATE},
			},
			context:  MOVE,
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsValidInContext(tt.pattern, tt.context); got != tt.expected {
				t.Errorf("IsValidInContext() = %v, want %v", got, tt.expected)
			}
		})
	}
}
