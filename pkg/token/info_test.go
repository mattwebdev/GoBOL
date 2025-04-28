package token

import (
	"testing"
)

func TestNewTokenInfo(t *testing.T) {
	tests := []struct {
		name     string
		typ      Token
		literal  string
		pos      Position
		expected TokenInfo
	}{
		{
			name:    "Create MOVE token info",
			typ:     MOVE,
			literal: "MOVE",
			pos:     Position{Line: 1, Column: 1},
			expected: TokenInfo{
				Type:    MOVE,
				Literal: "MOVE",
				Pos:     Position{Line: 1, Column: 1},
			},
		},
		{
			name:    "Create ADD token info",
			typ:     ADD,
			literal: "ADD",
			pos:     Position{Line: 2, Column: 5},
			expected: TokenInfo{
				Type:    ADD,
				Literal: "ADD",
				Pos:     Position{Line: 2, Column: 5},
			},
		},
		{
			name:    "Create IDENTIFIER token info",
			typ:     IDENTIFIER,
			literal: "MY-VAR",
			pos:     Position{Line: 3, Column: 10},
			expected: TokenInfo{
				Type:    IDENTIFIER,
				Literal: "MY-VAR",
				Pos:     Position{Line: 3, Column: 10},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := NewTokenInfo(tt.typ, tt.literal, tt.pos)
			if got != tt.expected {
				t.Errorf("NewTokenInfo() = %v, want %v", got, tt.expected)
			}
		})
	}
}

func TestTokenInfo_String(t *testing.T) {
	tests := []struct {
		name     string
		info     TokenInfo
		expected string
	}{
		{
			name: "MOVE token string",
			info: TokenInfo{
				Type:    MOVE,
				Literal: "MOVE",
				Pos:     Position{Line: 1, Column: 1},
			},
			expected: "Token{Type: MOVE, Literal: MOVE, Position: 1:1}",
		},
		{
			name: "ADD token string",
			info: TokenInfo{
				Type:    ADD,
				Literal: "ADD",
				Pos:     Position{Line: 2, Column: 5},
			},
			expected: "Token{Type: ADD, Literal: ADD, Position: 2:5}",
		},
		{
			name: "IDENTIFIER token string",
			info: TokenInfo{
				Type:    IDENTIFIER,
				Literal: "MY-VAR",
				Pos:     Position{Line: 3, Column: 10},
			},
			expected: "Token{Type: IDENTIFIER, Literal: MY-VAR, Position: 3:10}",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.info.String(); got != tt.expected {
				t.Errorf("TokenInfo.String() = %v, want %v", got, tt.expected)
			}
		})
	}
}
