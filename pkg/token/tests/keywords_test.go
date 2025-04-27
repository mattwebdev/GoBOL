package token

import (
	"testing"
)

func TestKeywordTokenValues(t *testing.T) {
	// Test that keyword tokens start at 600
	if GO < 600 {
		t.Errorf("Expected keyword tokens to start at 600, but GO token is %d", GO)
	}

	// Test that all keyword tokens are unique
	seen := make(map[Token]string)
	tokens := []struct {
		tok  Token
		name string
	}{
		{GO, "GO"},
		{TO, "TO"},
		{THAN, "THAN"},
		{OR, "OR"},
		{AND, "AND"},
		{IS, "IS"},
		{NOT, "NOT"},
		{IN, "IN"},
		{BY, "BY"},
		{FROM, "FROM"},
		{GIVING, "GIVING"},
		{IDENTIFICATION, "IDENTIFICATION"},
		{ENVIRONMENT, "ENVIRONMENT"},
		{DATA, "DATA"},
		{PROCEDURE, "PROCEDURE"},
		{DIVISION, "DIVISION"},
		{SECTION, "SECTION"},
		{PICTURE, "PICTURE"},
		{PIC, "PIC"},
		{USAGE, "USAGE"},
		{VALUE, "VALUE"},
		{VALUES, "VALUES"},
		{OCCURS, "OCCURS"},
		{TIMES, "TIMES"},
		{DEPENDING, "DEPENDING"},
		{ON, "ON"},
		{INDEXED, "INDEXED"},
		{VARYING, "VARYING"},
		{UNTIL, "UNTIL"},
		{GREATER, "GREATER"},
		{LESS, "LESS"},
		{EQUAL, "EQUAL"},
		{EQUALS, "EQUALS"},
		{CORRESPONDING, "CORRESPONDING"},
		{CORR, "CORR"},
	}

	for _, tt := range tokens {
		if existing, exists := seen[tt.tok]; exists {
			t.Errorf("Token value collision between %s and %s", tt.name, existing)
		}
		seen[tt.tok] = tt.name
	}
}

func TestIsKeyword(t *testing.T) {
	tests := []struct {
		token    Token
		expected bool
	}{
		{CORRESPONDING, true},
		{TO, true},
		{GIVING, true},
		{GREATER, true},
		{ILLEGAL, false},
		{EOF, false},
		{IDENTIFIER, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := IsKeyword(tt.token); got != tt.expected {
				t.Errorf("IsKeyword(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestGetKeywordInfo(t *testing.T) {
	tests := []struct {
		token           Token
		expectedExists  bool
		expectedClass   TokenClass
		expectedAliases []string
	}{
		{
			token:           CORRESPONDING,
			expectedExists:  true,
			expectedClass:   CLASS_MODIFIER,
			expectedAliases: []string{"CORR"},
		},
		{
			token:          TO,
			expectedExists: true,
			expectedClass:  CLASS_KEYWORD,
		},
		{
			token:          ILLEGAL,
			expectedExists: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			info, exists := GetKeywordInfo(tt.token)
			if exists != tt.expectedExists {
				t.Errorf("GetKeywordInfo(%v) exists = %v, want %v", tt.token, exists, tt.expectedExists)
				return
			}
			if !exists {
				return
			}
			if info.Class != tt.expectedClass {
				t.Errorf("GetKeywordInfo(%v) class = %v, want %v", tt.token, info.Class, tt.expectedClass)
			}
			if tt.expectedAliases != nil {
				if len(info.Aliases) != len(tt.expectedAliases) {
					t.Errorf("GetKeywordInfo(%v) aliases length = %v, want %v", tt.token, len(info.Aliases), len(tt.expectedAliases))
				} else {
					for i, alias := range tt.expectedAliases {
						if info.Aliases[i] != alias {
							t.Errorf("GetKeywordInfo(%v) alias[%d] = %v, want %v", tt.token, i, info.Aliases[i], alias)
						}
					}
				}
			}
		})
	}
}
