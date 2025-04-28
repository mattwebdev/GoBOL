package token

import (
	"testing"
)

func TestValidateTokenContext(t *testing.T) {
	tests := []struct {
		name    string
		tok     Token
		context []Token
		wantErr bool
	}{
		{
			name:    "valid verb in procedure division",
			tok:     MOVE,
			context: []Token{PROCEDURE_DIVISION},
			wantErr: false,
		},
		{
			name:    "invalid verb in identification division",
			tok:     MOVE,
			context: []Token{IDENTIFICATION_DIVISION},
			wantErr: true,
		},
		{
			name:    "valid keyword in data division",
			tok:     PICTURE,
			context: []Token{DATA_DIVISION},
			wantErr: false,
		},
		{
			name:    "invalid keyword in procedure division",
			tok:     PICTURE,
			context: []Token{PROCEDURE_DIVISION},
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := ValidateTokenContext(tt.tok, tt.context)
			if (err != nil) != tt.wantErr {
				t.Errorf("ValidateTokenContext() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestValidateTokenGroup(t *testing.T) {
	tests := []struct {
		name  string
		group *TokenGroup
		want  bool
	}{
		{
			name: "valid MOVE statement",
			group: func() *TokenGroup {
				g := NewTokenGroup(TokenInfo{Type: MOVE})
				g.AddParameter(TokenInfo{Type: IDENTIFIER})
				g.AddParameter(TokenInfo{Type: TO})
				g.AddParameter(TokenInfo{Type: IDENTIFIER})
				g.AddTerminator(TokenInfo{Type: PERIOD})
				return g
			}(),
			want: true,
		},
		{
			name: "invalid MOVE statement (missing TO)",
			group: func() *TokenGroup {
				g := NewTokenGroup(TokenInfo{Type: MOVE})
				g.AddParameter(TokenInfo{Type: IDENTIFIER})
				g.AddParameter(TokenInfo{Type: IDENTIFIER})
				g.AddTerminator(TokenInfo{Type: PERIOD})
				return g
			}(),
			want: false,
		},
		{
			name: "valid IF statement",
			group: func() *TokenGroup {
				g := NewTokenGroup(TokenInfo{Type: IF})
				g.AddParameter(TokenInfo{Type: IDENTIFIER})
				g.AddParameter(TokenInfo{Type: EQUAL})
				g.AddParameter(TokenInfo{Type: NUMBER_LIT})
				g.AddParameter(TokenInfo{Type: THEN})
				g.AddParameter(TokenInfo{Type: MOVE})
				g.AddParameter(TokenInfo{Type: IDENTIFIER})
				g.AddParameter(TokenInfo{Type: TO})
				g.AddParameter(TokenInfo{Type: IDENTIFIER})
				g.AddTerminator(TokenInfo{Type: PERIOD})
				return g
			}(),
			want: true,
		},
		{
			name: "invalid IF statement (missing THEN)",
			group: func() *TokenGroup {
				g := NewTokenGroup(TokenInfo{Type: IF})
				g.AddParameter(TokenInfo{Type: IDENTIFIER})
				g.AddParameter(TokenInfo{Type: EQUAL})
				g.AddParameter(TokenInfo{Type: NUMBER_LIT})
				g.AddParameter(TokenInfo{Type: MOVE})
				g.AddParameter(TokenInfo{Type: IDENTIFIER})
				g.AddParameter(TokenInfo{Type: TO})
				g.AddParameter(TokenInfo{Type: IDENTIFIER})
				g.AddTerminator(TokenInfo{Type: PERIOD})
				return g
			}(),
			want: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokens := []Token{tt.group.MainToken.Type}
			for _, p := range tt.group.Parameters {
				tokens = append(tokens, p.Type)
			}
			err := ValidateTokenGroup(tokens, []Token{PROCEDURE_DIVISION})
			if (err == nil) != tt.want {
				t.Errorf("ValidateTokenGroup() error = %v, want %v", err, tt.want)
			}
		})
	}
}

func TestIsValidTokenInContext(t *testing.T) {
	tests := []struct {
		token    Token
		context  []Token
		expected bool
	}{
		{PICTURE, []Token{DATA_DIVISION}, true},
		{PICTURE, []Token{PROCEDURE_DIVISION}, false},
		{ILLEGAL, []Token{DATA_DIVISION}, true},
	}

	for _, test := range tests {
		result := isValidTokenInContext(test.token, test.context)
		if result != test.expected {
			t.Errorf("isValidTokenInContext(%v, %v) = %v, want %v", test.token, test.context, result, test.expected)
		}
	}
}
