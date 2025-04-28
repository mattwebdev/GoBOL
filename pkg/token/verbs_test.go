package token

import (
	"testing"
)

func TestVerbTokenValues(t *testing.T) {
	// Test that verb tokens start at 100
	if MOVE < 100 {
		t.Errorf("Expected verb tokens to start at 100, but MOVE token is %d", MOVE)
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
		{STRING_VERB, "STRING"},
		{UNSTRING, "UNSTRING"},
		{INSPECT, "INSPECT"},
		{SEARCH, "SEARCH"},
		{SET, "SET"},
		{SORT, "SORT"},
		{MERGE, "MERGE"},
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
		token            Token
		expectedExists   bool
		expectedCategory string
		expectedEndToken Token
	}{
		{
			token:            MOVE,
			expectedExists:   true,
			expectedCategory: "data movement",
		},
		{
			token:            ADD,
			expectedExists:   true,
			expectedCategory: "arithmetic",
			expectedEndToken: END_ADD,
		},
		{
			token:            INITIALIZE,
			expectedExists:   true,
			expectedCategory: "data movement",
			expectedEndToken: END_INITIALIZE,
		},
		{
			token:            USE,
			expectedExists:   true,
			expectedCategory: "procedure",
		},
		{
			token:            RELEASE,
			expectedExists:   true,
			expectedCategory: "i/o",
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
			if info.Category != tt.expectedCategory {
				t.Errorf("GetVerbInfo(%v) category = %v, want %v", tt.token, info.Category, tt.expectedCategory)
			}
			if info.EndToken != tt.expectedEndToken {
				t.Errorf("GetVerbInfo(%v) endToken = %v, want %v", tt.token, info.EndToken, tt.expectedEndToken)
			}
		})
	}
}
