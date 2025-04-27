package token

import (
	"testing"
)

func TestFigurativeConstantTokenValues(t *testing.T) {
	// Test that figurative constant tokens start at 300
	if ZERO < 300 {
		t.Errorf("Expected figurative constant tokens to start at 300, but ZERO token is %d", ZERO)
	}

	// Test that all figurative constant tokens are unique
	seen := make(map[Token]string)
	tokens := []struct {
		tok  Token
		name string
	}{
		{ZERO, "ZERO"},
		{ZEROS, "ZEROS"},
		{ZEROES, "ZEROES"},
		{SPACE, "SPACE"},
		{SPACES, "SPACES"},
		{HIGH_VALUE, "HIGH_VALUE"},
		{HIGH_VALUES, "HIGH_VALUES"},
		{LOW_VALUE, "LOW_VALUE"},
		{LOW_VALUES, "LOW_VALUES"},
		{QUOTE, "QUOTE"},
		{QUOTES, "QUOTES"},
		{NULL, "NULL"},
		{NULLS, "NULLS"},
		{ALL, "ALL"},
	}

	for _, tt := range tokens {
		if existing, exists := seen[tt.tok]; exists {
			t.Errorf("Token value collision between %s and %s", tt.name, existing)
		}
		seen[tt.tok] = tt.name
	}
}

func TestIsFigurativeConstant(t *testing.T) {
	tests := []struct {
		token    Token
		expected bool
	}{
		{ZERO, true},
		{ZEROS, false}, // alias, not primary
		{SPACE, true},
		{SPACES, false}, // alias, not primary
		{HIGH_VALUE, true},
		{LOW_VALUE, true},
		{ALL, false}, // special modifier, not a constant
		{ILLEGAL, false},
		{EOF, false},
		{IDENTIFIER, false},
		{STRING_LIT, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := IsFigurativeConstant(tt.token); got != tt.expected {
				t.Errorf("IsFigurativeConstant(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestGetFigurativeConstantInfo(t *testing.T) {
	tests := []struct {
		token             Token
		expectedExists    bool
		expectedType      LiteralType
		expectedValue     string
		expectedAliases   []Token
		expectedCanUseALL bool
	}{
		{
			token:             ZERO,
			expectedExists:    true,
			expectedType:      LIT_FIGURATIVE,
			expectedValue:     "0",
			expectedAliases:   []Token{ZEROS, ZEROES},
			expectedCanUseALL: true,
		},
		{
			token:             SPACE,
			expectedExists:    true,
			expectedType:      LIT_FIGURATIVE,
			expectedValue:     " ",
			expectedAliases:   []Token{SPACES},
			expectedCanUseALL: true,
		},
		{
			token:             HIGH_VALUE,
			expectedExists:    true,
			expectedType:      LIT_FIGURATIVE,
			expectedValue:     "\xFF",
			expectedAliases:   []Token{HIGH_VALUES},
			expectedCanUseALL: true,
		},
		{
			token:          ILLEGAL,
			expectedExists: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			info, exists := GetFigurativeConstantInfo(tt.token)
			if exists != tt.expectedExists {
				t.Errorf("GetFigurativeConstantInfo(%v) exists = %v, want %v", tt.token, exists, tt.expectedExists)
				return
			}
			if !exists {
				return
			}
			if info.Type != tt.expectedType {
				t.Errorf("GetFigurativeConstantInfo(%v) type = %v, want %v", tt.token, info.Type, tt.expectedType)
			}
			if info.Value != tt.expectedValue {
				t.Errorf("GetFigurativeConstantInfo(%v) value = %v, want %v", tt.token, info.Value, tt.expectedValue)
			}
			if info.CanUseALL != tt.expectedCanUseALL {
				t.Errorf("GetFigurativeConstantInfo(%v) canUseALL = %v, want %v", tt.token, info.CanUseALL, tt.expectedCanUseALL)
			}
			if tt.expectedAliases != nil {
				if len(info.Aliases) != len(tt.expectedAliases) {
					t.Errorf("GetFigurativeConstantInfo(%v) aliases length = %v, want %v", tt.token, len(info.Aliases), len(tt.expectedAliases))
				} else {
					for i, alias := range tt.expectedAliases {
						if info.Aliases[i] != alias {
							t.Errorf("GetFigurativeConstantInfo(%v) alias[%d] = %v, want %v", tt.token, i, info.Aliases[i], alias)
						}
					}
				}
			}
		})
	}
}

func TestIsLiteral(t *testing.T) {
	tests := []struct {
		token    Token
		expected bool
	}{
		{STRING_LIT, true},
		{NUMBER_LIT, true},
		{ZERO, true},
		{SPACE, true},
		{HIGH_VALUE, true},
		{LOW_VALUE, true},
		{ZEROS, false},  // alias, not primary
		{SPACES, false}, // alias, not primary
		{ALL, false},    // modifier, not literal
		{ILLEGAL, false},
		{EOF, false},
		{IDENTIFIER, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := IsLiteral(tt.token); got != tt.expected {
				t.Errorf("IsLiteral(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}
