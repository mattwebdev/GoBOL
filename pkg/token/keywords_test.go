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
		{WHEN, "WHEN"},
		{THROUGH, "THROUGH"},
		{THRU, "THRU"},
		{AFTER, "AFTER"},
		{BEFORE, "BEFORE"},
		{STANDARD, "STANDARD"},
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
		{BINARY, "BINARY"},
		{COMPUTATIONAL, "COMPUTATIONAL"},
		{COMP, "COMP"},
		{COMP_1, "COMP_1"},
		{COMP_2, "COMP_2"},
		{COMP_3, "COMP_3"},
		{COMP_4, "COMP_4"},
		{COMP_5, "COMP_5"},
		{SYNCHRONIZED, "SYNCHRONIZED"},
		{SYNC, "SYNC"},
		{JUSTIFIED, "JUSTIFIED"},
		{JUST, "JUST"},
		{BLANK, "BLANK"},
		{RENAMES, "RENAMES"},
		{REDEFINES, "REDEFINES"},
		{FILLER, "FILLER"},
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
		{BINARY, true},
		{COMPUTATIONAL, true},
		{COMP, true},
		{SYNCHRONIZED, true},
		{SYNC, true},
		{JUSTIFIED, true},
		{JUST, true},
		{BLANK, true},
		{RENAMES, true},
		{REDEFINES, true},
		{FILLER, true},
		{THROUGH, true},
		{THRU, true},
		{WHEN, true},
		{AFTER, true},
		{BEFORE, true},
		{STANDARD, true},
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
		token            Token
		expectedExists   bool
		expectedClass    TokenClass
		expectedAliases  []string
		expectedCategory string
	}{
		{
			token:            CORRESPONDING,
			expectedExists:   true,
			expectedClass:    CLASS_MODIFIER,
			expectedAliases:  []string{"CORR"},
			expectedCategory: "modifier",
		},
		{
			token:            COMPUTATIONAL,
			expectedExists:   true,
			expectedClass:    CLASS_KEYWORD,
			expectedAliases:  []string{"COMP"},
			expectedCategory: "usage",
		},
		{
			token:            SYNCHRONIZED,
			expectedExists:   true,
			expectedClass:    CLASS_KEYWORD,
			expectedAliases:  []string{"SYNC"},
			expectedCategory: "alignment",
		},
		{
			token:            JUSTIFIED,
			expectedExists:   true,
			expectedClass:    CLASS_KEYWORD,
			expectedAliases:  []string{"JUST"},
			expectedCategory: "alignment",
		},
		{
			token:            THROUGH,
			expectedExists:   true,
			expectedClass:    CLASS_KEYWORD,
			expectedAliases:  []string{"THRU"},
			expectedCategory: "range",
		},
		{
			token:            WHEN,
			expectedExists:   true,
			expectedClass:    CLASS_KEYWORD,
			expectedCategory: "control",
		},
		{
			token:            AFTER,
			expectedExists:   true,
			expectedClass:    CLASS_KEYWORD,
			expectedCategory: "position",
		},
		{
			token:            BEFORE,
			expectedExists:   true,
			expectedClass:    CLASS_KEYWORD,
			expectedCategory: "position",
		},
		{
			token:            STANDARD,
			expectedExists:   true,
			expectedClass:    CLASS_KEYWORD,
			expectedCategory: "qualifier",
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
			if tt.expectedCategory != "" && info.Category != tt.expectedCategory {
				t.Errorf("GetKeywordInfo(%v) category = %v, want %v", tt.token, info.Category, tt.expectedCategory)
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
