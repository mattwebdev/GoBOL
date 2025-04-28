package token

import (
	"testing"
)

func TestConditionTokenValues(t *testing.T) {
	// Test that condition tokens have expected values
	tests := []struct {
		tok  Token
		name string
	}{
		{ON_EXCEPTION, "ON EXCEPTION"},
		{NOT_ON_EXCEPTION, "NOT ON EXCEPTION"},
		{ON_OVERFLOW, "ON OVERFLOW"},
		{NOT_ON_OVERFLOW, "NOT ON OVERFLOW"},
		{ON_SIZE_ERROR, "ON SIZE ERROR"},
		{NOT_ON_SIZE_ERROR, "NOT ON SIZE ERROR"},
	}

	// Test that all condition tokens are unique
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

func TestIsCondition(t *testing.T) {
	tests := []struct {
		name     string
		token    Token
		expected bool
	}{
		{"ON_EXCEPTION is condition", ON_EXCEPTION, true},
		{"NOT_ON_EXCEPTION is condition", NOT_ON_EXCEPTION, true},
		{"ON_OVERFLOW is condition", ON_OVERFLOW, true},
		{"NOT_ON_OVERFLOW is condition", NOT_ON_OVERFLOW, true},
		{"ON_SIZE_ERROR is condition", ON_SIZE_ERROR, true},
		{"NOT_ON_SIZE_ERROR is condition", NOT_ON_SIZE_ERROR, true},
		{"MOVE is not condition", MOVE, false},
		{"ADD is not condition", ADD, false},
		{"ILLEGAL is not condition", ILLEGAL, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsCondition(tt.token); got != tt.expected {
				t.Errorf("IsCondition(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestGetConditionInfo(t *testing.T) {
	tests := []struct {
		name         string
		token        Token
		wantClass    TokenClass
		wantCategory string
		wantContext  []Token
		wantOk       bool
	}{
		{
			name:         "ON_EXCEPTION info",
			token:        ON_EXCEPTION,
			wantClass:    CLASS_CONDITION,
			wantCategory: "condition",
			wantContext:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantOk:       true,
		},
		{
			name:         "ON_OVERFLOW info",
			token:        ON_OVERFLOW,
			wantClass:    CLASS_CONDITION,
			wantCategory: "condition",
			wantContext:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantOk:       true,
		},
		{
			name:         "ON_SIZE_ERROR info",
			token:        ON_SIZE_ERROR,
			wantClass:    CLASS_CONDITION,
			wantCategory: "condition",
			wantContext:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantOk:       true,
		},
		{
			name:   "MOVE has no condition info",
			token:  MOVE,
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotInfo, gotOk := GetConditionInfo(tt.token)
			if gotOk != tt.wantOk {
				t.Errorf("GetConditionInfo(%v) ok = %v, want %v", tt.token, gotOk, tt.wantOk)
			}
			if gotOk {
				if gotInfo.Class != tt.wantClass {
					t.Errorf("GetConditionInfo(%v).Class = %v, want %v", tt.token, gotInfo.Class, tt.wantClass)
				}
				if gotInfo.Category != tt.wantCategory {
					t.Errorf("GetConditionInfo(%v).Category = %v, want %v", tt.token, gotInfo.Category, tt.wantCategory)
				}
				if len(gotInfo.Context) != len(tt.wantContext) {
					t.Errorf("GetConditionInfo(%v).Context length = %v, want %v", tt.token, len(gotInfo.Context), len(tt.wantContext))
				} else {
					for i, v := range gotInfo.Context {
						if v != tt.wantContext[i] {
							t.Errorf("GetConditionInfo(%v).Context[%d] = %v, want %v", tt.token, i, v, tt.wantContext[i])
						}
					}
				}
			}
		})
	}
}
