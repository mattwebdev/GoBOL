package token

import (
	"testing"
)

func TestVerbTokenValues(t *testing.T) {
	// Test that verb tokens start at 700
	if MOVE < 700 {
		t.Errorf("Expected verb tokens to start at 700, but MOVE token is %d", MOVE)
	}

	// Test that all verb tokens are unique
	seen := make(map[Token]string)
	tokens := []struct {
		tok  Token
		name string
	}{
		{MOVE, "MOVE"},
		{ADD, "ADD"},
		{SUBTRACT, "SUBTRACT"},
		{MULTIPLY, "MULTIPLY"},
		{DIVIDE, "DIVIDE"},
		{COMPUTE, "COMPUTE"},
		{INITIALIZE, "INITIALIZE"},
		{ACCEPT, "ACCEPT"},
		{DISPLAY, "DISPLAY"},
		{READ, "READ"},
		{WRITE, "WRITE"},
		{REWRITE, "REWRITE"},
		{DELETE, "DELETE"},
		{START, "START"},
		{OPEN, "OPEN"},
		{CLOSE, "CLOSE"},
		{RELEASE, "RELEASE"},
		{RETURN, "RETURN"},
		{PERFORM, "PERFORM"},
		{GO_TO, "GO_TO"},
		{IF, "IF"},
		{EVALUATE, "EVALUATE"},
		{CONTINUE, "CONTINUE"},
		{EXIT, "EXIT"},
		{STOP, "STOP"},
		{ALTER, "ALTER"},
		{USE, "USE"},
		{RAISE, "RAISE"},
		{RESUME, "RESUME"},
		{STRING_VERB, "STRING_VERB"},
		{UNSTRING, "UNSTRING"},
		{INSPECT, "INSPECT"},
		{SEARCH, "SEARCH"},
		{SET, "SET"},
		{SORT, "SORT"},
		{MERGE, "MERGE"},
		{ALLOCATE, "ALLOCATE"},
		{FREE, "FREE"},
		{VALIDATE, "VALIDATE"},
		{GENERATE, "GENERATE"},
		{SUPPRESS, "SUPPRESS"},
		{CALL, "CALL"},
		{CANCEL, "CANCEL"},
		{GOBACK, "GOBACK"},
		{EXIT_PROGRAM, "EXIT_PROGRAM"},
	}

	for _, tt := range tokens {
		if existing, exists := seen[tt.tok]; exists {
			t.Errorf("Token value collision between %s and %s", tt.name, existing)
		}
		seen[tt.tok] = tt.name
	}
}

func TestIsVerb(t *testing.T) {
	tests := []struct {
		token    Token
		expected bool
	}{
		{MOVE, true},
		{ADD, true},
		{PERFORM, true},
		{INITIALIZE, true},
		{RELEASE, true},
		{RETURN, true},
		{ALTER, true},
		{USE, true},
		{GENERATE, true},
		{SUPPRESS, true},
		{RAISE, true},
		{RESUME, true},
		{ALLOCATE, true},
		{FREE, true},
		{VALIDATE, true},
		{ILLEGAL, false},
		{EOF, false},
		{IDENTIFIER, false},
		{CORRESPONDING, false}, // keyword, not verb
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := IsVerb(tt.token); got != tt.expected {
				t.Errorf("IsVerb(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestGetVerbInfo(t *testing.T) {
	tests := []struct {
		token             Token
		expectedExists    bool
		expectedClass     TokenClass
		expectedMinParams int
		expectedMaxParams int
		expectedModifiers []Token
	}{
		{
			token:             MOVE,
			expectedExists:    true,
			expectedClass:     CLASS_VERB,
			expectedMinParams: 2,
			expectedMaxParams: 2,
			expectedModifiers: []Token{CORRESPONDING, TO},
		},
		{
			token:             ADD,
			expectedExists:    true,
			expectedClass:     CLASS_VERB,
			expectedMinParams: 2,
			expectedMaxParams: -1, // unlimited
			expectedModifiers: []Token{TO, GIVING},
		},
		{
			token:             INITIALIZE,
			expectedExists:    true,
			expectedClass:     CLASS_VERB,
			expectedMinParams: 1,
			expectedMaxParams: -1,
			expectedModifiers: []Token{TO, VALUE},
		},
		{
			token:             USE,
			expectedExists:    true,
			expectedClass:     CLASS_VERB,
			expectedMinParams: 1,
			expectedMaxParams: -1,
			expectedModifiers: nil,
		},
		{
			token:             RELEASE,
			expectedExists:    true,
			expectedClass:     CLASS_VERB,
			expectedMinParams: 1,
			expectedMaxParams: 1,
			expectedModifiers: nil,
		},
		{
			token:             RAISE,
			expectedExists:    true,
			expectedClass:     CLASS_VERB,
			expectedMinParams: 1,
			expectedMaxParams: 1,
			expectedModifiers: nil,
		},
		{
			token:             RESUME,
			expectedExists:    true,
			expectedClass:     CLASS_VERB,
			expectedMinParams: 0,
			expectedMaxParams: 1,
			expectedModifiers: nil,
		},
		{
			token:             ALLOCATE,
			expectedExists:    true,
			expectedClass:     CLASS_VERB,
			expectedMinParams: 1,
			expectedMaxParams: -1,
			expectedModifiers: nil,
		},
		{
			token:             FREE,
			expectedExists:    true,
			expectedClass:     CLASS_VERB,
			expectedMinParams: 1,
			expectedMaxParams: -1,
			expectedModifiers: nil,
		},
		{
			token:             VALIDATE,
			expectedExists:    true,
			expectedClass:     CLASS_VERB,
			expectedMinParams: 1,
			expectedMaxParams: -1,
			expectedModifiers: nil,
		},
		{
			token:          ILLEGAL,
			expectedExists: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			info, exists := GetVerbInfo(tt.token)
			if exists != tt.expectedExists {
				t.Errorf("GetVerbInfo(%v) exists = %v, want %v", tt.token, exists, tt.expectedExists)
				return
			}
			if !exists {
				return
			}
			if info.Class != tt.expectedClass {
				t.Errorf("GetVerbInfo(%v) class = %v, want %v", tt.token, info.Class, tt.expectedClass)
			}
			if info.MinParams != tt.expectedMinParams {
				t.Errorf("GetVerbInfo(%v) minParams = %v, want %v", tt.token, info.MinParams, tt.expectedMinParams)
			}
			if info.MaxParams != tt.expectedMaxParams {
				t.Errorf("GetVerbInfo(%v) maxParams = %v, want %v", tt.token, info.MaxParams, tt.expectedMaxParams)
			}
			if tt.expectedModifiers != nil {
				if len(info.Modifiers) != len(tt.expectedModifiers) {
					t.Errorf("GetVerbInfo(%v) modifiers length = %v, want %v", tt.token, len(info.Modifiers), len(tt.expectedModifiers))
				} else {
					for i, modifier := range tt.expectedModifiers {
						if info.Modifiers[i] != modifier {
							t.Errorf("GetVerbInfo(%v) modifier[%d] = %v, want %v", tt.token, i, info.Modifiers[i], modifier)
						}
					}
				}
			}
		})
	}
}

func TestGetVerbPatterns(t *testing.T) {
	tests := []struct {
		token            Token
		expectedPatterns []MultiWordPattern
	}{
		{
			token: MOVE,
			expectedPatterns: []MultiWordPattern{
				{
					Parts:    []Token{MOVE, CORRESPONDING, TO},
					Result:   MOVE,
					Optional: []bool{false, false, false},
				},
				{
					Parts:    []Token{MOVE, CORR, TO},
					Result:   MOVE,
					Optional: []bool{false, false, false},
				},
			},
		},
		{
			token: PERFORM,
			expectedPatterns: []MultiWordPattern{
				{
					Parts:    []Token{PERFORM, TIMES},
					Result:   PERFORM,
					Optional: []bool{false, false},
				},
				{
					Parts:    []Token{PERFORM, UNTIL},
					Result:   PERFORM,
					Optional: []bool{false, false},
				},
				{
					Parts:    []Token{PERFORM, VARYING},
					Result:   PERFORM,
					Optional: []bool{false, false},
				},
			},
		},
		{
			token: INITIALIZE,
			expectedPatterns: []MultiWordPattern{
				{
					Parts:    []Token{INITIALIZE, TO, VALUE},
					Result:   INITIALIZE,
					Optional: []bool{false, false, false},
				},
			},
		},
		{
			token: USE,
			expectedPatterns: []MultiWordPattern{
				{
					Parts:    []Token{USE, AFTER, STANDARD},
					Result:   USE,
					Optional: []bool{false, false, false},
				},
				{
					Parts:    []Token{USE, BEFORE, STANDARD},
					Result:   USE,
					Optional: []bool{false, false, false},
				},
			},
		},
		{
			token: RELEASE,
			expectedPatterns: []MultiWordPattern{
				{
					Parts:    []Token{RELEASE, FROM},
					Result:   RELEASE,
					Optional: []bool{false, true},
				},
			},
		},
		{
			token: ALLOCATE,
			expectedPatterns: []MultiWordPattern{
				{
					Parts:    []Token{ALLOCATE, BASED},
					Result:   ALLOCATE,
					Optional: []bool{false, true},
				},
			},
		},
		{
			token:            RAISE,
			expectedPatterns: nil,
		},
		{
			token:            RESUME,
			expectedPatterns: nil,
		},
		{
			token:            FREE,
			expectedPatterns: nil,
		},
		{
			token:            VALIDATE,
			expectedPatterns: nil,
		},
		{
			token:            ILLEGAL,
			expectedPatterns: nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			patterns := GetVerbPatterns(tt.token)
			if tt.expectedPatterns == nil {
				if patterns != nil {
					t.Errorf("GetVerbPatterns(%v) = %v, want nil", tt.token, patterns)
				}
				return
			}
			if len(patterns) != len(tt.expectedPatterns) {
				t.Errorf("GetVerbPatterns(%v) length = %v, want %v", tt.token, len(patterns), len(tt.expectedPatterns))
				return
			}
			for i, expectedPattern := range tt.expectedPatterns {
				pattern := patterns[i]
				if len(pattern.Parts) != len(expectedPattern.Parts) {
					t.Errorf("Pattern[%d].Parts length = %v, want %v", i, len(pattern.Parts), len(expectedPattern.Parts))
					continue
				}
				for j, part := range expectedPattern.Parts {
					if pattern.Parts[j] != part {
						t.Errorf("Pattern[%d].Parts[%d] = %v, want %v", i, j, pattern.Parts[j], part)
					}
				}
				if pattern.Result != expectedPattern.Result {
					t.Errorf("Pattern[%d].Result = %v, want %v", i, pattern.Result, expectedPattern.Result)
				}
				if len(pattern.Optional) != len(expectedPattern.Optional) {
					t.Errorf("Pattern[%d].Optional length = %v, want %v", i, len(pattern.Optional), len(expectedPattern.Optional))
					continue
				}
				for j, opt := range expectedPattern.Optional {
					if pattern.Optional[j] != opt {
						t.Errorf("Pattern[%d].Optional[%d] = %v, want %v", i, j, pattern.Optional[j], opt)
					}
				}
			}
		})
	}
}
