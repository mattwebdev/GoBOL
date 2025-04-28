package token

import (
	"testing"
)

func TestScopeTokenValues(t *testing.T) {
	// Test that all scope tokens are unique
	tests := []struct {
		tok  Token
		name string
	}{
		{END_START, "END-START"},
		{END_DELETE, "END-DELETE"},
		{END_REWRITE, "END-REWRITE"},
		{END_RETURN, "END-RETURN"},
		{END_INITIALIZE, "END-INITIALIZE"},
	}

	// Test that all scope tokens are unique
	seen := make(map[Token]string)
	for _, tt := range tests {
		if existing, exists := seen[tt.tok]; exists {
			t.Errorf("Token value collision between %s and %s", tt.name, existing)
		}
		seen[tt.tok] = tt.name
	}

	// Test that token strings match expected values
	for _, tt := range tests {
		if got := tt.tok.String(); got != tt.name {
			t.Errorf("Token %d String() = %q, want %q", tt.tok, got, tt.name)
		}
	}
}

func TestIsScope(t *testing.T) {
	tests := []struct {
		name     string
		token    Token
		expected bool
	}{
		{"END_START is scope", END_START, true},
		{"END_DELETE is scope", END_DELETE, true},
		{"END_REWRITE is scope", END_REWRITE, true},
		{"END_RETURN is scope", END_RETURN, true},
		{"END_INITIALIZE is scope", END_INITIALIZE, true},
		{"MOVE is not scope", MOVE, false},
		{"ADD is not scope", ADD, false},
		{"ILLEGAL is not scope", ILLEGAL, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsScope(tt.token); got != tt.expected {
				t.Errorf("IsScope(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestGetScopeInfo(t *testing.T) {
	tests := []struct {
		name         string
		token        Token
		wantClass    TokenClass
		wantCategory string
		wantContext  []Token
		wantOk       bool
	}{
		{
			name:         "END_START info",
			token:        END_START,
			wantClass:    CLASS_SCOPE,
			wantCategory: "scope terminator",
			wantContext:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantOk:       true,
		},
		{
			name:         "END_DELETE info",
			token:        END_DELETE,
			wantClass:    CLASS_SCOPE,
			wantCategory: "scope terminator",
			wantContext:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantOk:       true,
		},
		{
			name:         "END_REWRITE info",
			token:        END_REWRITE,
			wantClass:    CLASS_SCOPE,
			wantCategory: "scope terminator",
			wantContext:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantOk:       true,
		},
		{
			name:         "END_RETURN info",
			token:        END_RETURN,
			wantClass:    CLASS_SCOPE,
			wantCategory: "scope terminator",
			wantContext:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantOk:       true,
		},
		{
			name:         "END_INITIALIZE info",
			token:        END_INITIALIZE,
			wantClass:    CLASS_SCOPE,
			wantCategory: "scope terminator",
			wantContext:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantOk:       true,
		},
		{
			name:   "MOVE has no scope info",
			token:  MOVE,
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotInfo, gotOk := GetScopeInfo(tt.token)
			if gotOk != tt.wantOk {
				t.Errorf("GetScopeInfo(%v) ok = %v, want %v", tt.token, gotOk, tt.wantOk)
			}
			if gotOk {
				if gotInfo.Class != tt.wantClass {
					t.Errorf("GetScopeInfo(%v).Class = %v, want %v", tt.token, gotInfo.Class, tt.wantClass)
				}
				if gotInfo.Category != tt.wantCategory {
					t.Errorf("GetScopeInfo(%v).Category = %v, want %v", tt.token, gotInfo.Category, tt.wantCategory)
				}
				if len(gotInfo.Context) != len(tt.wantContext) {
					t.Errorf("GetScopeInfo(%v).Context length = %v, want %v", tt.token, len(gotInfo.Context), len(tt.wantContext))
				} else {
					for i, v := range gotInfo.Context {
						if v != tt.wantContext[i] {
							t.Errorf("GetScopeInfo(%v).Context[%d] = %v, want %v", tt.token, i, v, tt.wantContext[i])
						}
					}
				}
			}
		})
	}
}
