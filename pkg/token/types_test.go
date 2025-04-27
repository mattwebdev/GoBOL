package token

import (
	"testing"
)

func TestTokenType_String(t *testing.T) {
	tests := []struct {
		tokenType TokenType
		expected  string
	}{
		{ILLEGAL, "ILLEGAL"},
		{EOF, "EOF"},
		{COMMENT, "COMMENT"},
		{IDENTIFIER, "IDENTIFIER"},
		{STRING_LIT, "STRING_LIT"},
		{NUMBER_LIT, "NUMBER_LIT"},
		{LEVEL_NUMBER, "LEVEL_NUMBER"},
	}

	for _, tt := range tests {
		t.Run(tt.expected, func(t *testing.T) {
			if got := tt.tokenType.String(); got != tt.expected {
				t.Errorf("TokenType.String() = %v, want %v", got, tt.expected)
			}
		})
	}
}

// TestTokenTypeValues ensures token type values are unique
func TestTokenTypeValues(t *testing.T) {
	seen := make(map[Token]string)
	tokens := []struct {
		tok  Token
		name string
	}{
		{ILLEGAL, "ILLEGAL"},
		{EOF, "EOF"},
		{COMMENT, "COMMENT"},
		{IDENTIFIER, "IDENTIFIER"},
		{STRING_LIT, "STRING_LIT"},
		{NUMBER_LIT, "NUMBER_LIT"},
		{LEVEL_NUMBER, "LEVEL_NUMBER"},
	}

	for _, tt := range tokens {
		if existing, exists := seen[tt.tok]; exists {
			t.Errorf("Token value collision between %s and %s", tt.name, existing)
		}
		seen[tt.tok] = tt.name
	}
}

func TestToken_String(t *testing.T) {
	tests := []struct {
		token    TokenInfo
		expected string
	}{
		{
			token: TokenInfo{
				Type:    IDENTIFIER,
				Literal: "MyVar",
				Pos: Position{
					Line:   1,
					Column: 1,
				},
			},
			expected: "Token{Type: IDENTIFIER, Literal: MyVar, Position: 1:1}",
		},
		{
			token: TokenInfo{
				Type:    NUMBER_LIT,
				Literal: "42",
				Pos: Position{
					Line:   5,
					Column: 10,
				},
			},
			expected: "Token{Type: NUMBER_LIT, Literal: 42, Position: 5:10}",
		},
	}

	for _, tt := range tests {
		t.Run(tt.expected, func(t *testing.T) {
			if got := tt.token.String(); got != tt.expected {
				t.Errorf("Token.String() = %v, want %v", got, tt.expected)
			}
		})
	}
}
