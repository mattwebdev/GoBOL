package token

import (
	"testing"
)

func TestOperatorTokenValues(t *testing.T) {
	// Test that operator tokens start at 500
	if OP_PLUS < 500 {
		t.Errorf("Expected operator tokens to start at 500, but OP_PLUS token is %d", OP_PLUS)
	}

	// Test that all operator tokens are unique
	seen := make(map[Token]string)
	tokens := []struct {
		tok  Token
		name string
	}{
		{OP_PLUS, "OP_PLUS"},
		{OP_MINUS, "OP_MINUS"},
		{OP_MULTIPLY, "OP_MULTIPLY"},
		{OP_DIVIDE, "OP_DIVIDE"},
		{OP_POWER, "OP_POWER"},
		{OP_EQUAL_TO, "OP_EQUAL_TO"},
		{OP_GREATER_THAN, "OP_GREATER_THAN"},
		{OP_LESS_THAN, "OP_LESS_THAN"},
		{OP_GREATER_EQUAL, "OP_GREATER_EQUAL"},
		{OP_LESS_EQUAL, "OP_LESS_EQUAL"},
		{OP_NOT_EQUAL, "OP_NOT_EQUAL"},
		{OP_AND, "OP_AND"},
		{OP_OR, "OP_OR"},
		{OP_NOT, "OP_NOT"},
		{OP_LPAREN, "OP_LPAREN"},
		{OP_RPAREN, "OP_RPAREN"},
		{OP_PERIOD, "OP_PERIOD"},
		{OP_COMMA, "OP_COMMA"},
	}

	for _, tt := range tokens {
		if existing, exists := seen[tt.tok]; exists {
			t.Errorf("Token value collision between %s and %s", tt.name, existing)
		}
		seen[tt.tok] = tt.name
	}
}

func TestIsOperator(t *testing.T) {
	tests := []struct {
		token    Token
		expected bool
	}{
		{OP_PLUS, true},
		{OP_MINUS, true},
		{OP_MULTIPLY, true},
		{OP_EQUAL_TO, true},
		{OP_AND, true},
		{ILLEGAL, false},
		{EOF, false},
		{IDENTIFIER, false},
		{MOVE, false}, // verb, not operator
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := IsOperator(tt.token); got != tt.expected {
				t.Errorf("IsOperator(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestGetOperatorInfo(t *testing.T) {
	tests := []struct {
		token              Token
		expectedExists     bool
		expectedType       OperatorType
		expectedPrecedence int
		expectedIsUnary    bool
		expectedIsBinary   bool
		expectedSymbol     string
		expectedKeywords   []string
	}{
		{
			token:              OP_PLUS,
			expectedExists:     true,
			expectedType:       OP_ARITHMETIC,
			expectedPrecedence: 10,
			expectedIsUnary:    true,
			expectedIsBinary:   true,
			expectedSymbol:     "+",
		},
		{
			token:              OP_EQUAL_TO,
			expectedExists:     true,
			expectedType:       OP_COMPARISON,
			expectedPrecedence: 5,
			expectedIsUnary:    false,
			expectedIsBinary:   true,
			expectedSymbol:     "=",
			expectedKeywords:   []string{"EQUAL", "EQUAL TO", "EQUALS"},
		},
		{
			token:          ILLEGAL,
			expectedExists: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			info, exists := GetOperatorInfo(tt.token)
			if exists != tt.expectedExists {
				t.Errorf("GetOperatorInfo(%v) exists = %v, want %v", tt.token, exists, tt.expectedExists)
				return
			}
			if !exists {
				return
			}
			if info.Type != tt.expectedType {
				t.Errorf("GetOperatorInfo(%v) type = %v, want %v", tt.token, info.Type, tt.expectedType)
			}
			if info.Precedence != tt.expectedPrecedence {
				t.Errorf("GetOperatorInfo(%v) precedence = %v, want %v", tt.token, info.Precedence, tt.expectedPrecedence)
			}
			if info.IsUnary != tt.expectedIsUnary {
				t.Errorf("GetOperatorInfo(%v) isUnary = %v, want %v", tt.token, info.IsUnary, tt.expectedIsUnary)
			}
			if info.IsBinary != tt.expectedIsBinary {
				t.Errorf("GetOperatorInfo(%v) isBinary = %v, want %v", tt.token, info.IsBinary, tt.expectedIsBinary)
			}
			if info.Symbol != tt.expectedSymbol {
				t.Errorf("GetOperatorInfo(%v) symbol = %v, want %v", tt.token, info.Symbol, tt.expectedSymbol)
			}
			if tt.expectedKeywords != nil {
				if len(info.Keywords) != len(tt.expectedKeywords) {
					t.Errorf("GetOperatorInfo(%v) keywords length = %v, want %v", tt.token, len(info.Keywords), len(tt.expectedKeywords))
				} else {
					for i, keyword := range tt.expectedKeywords {
						if info.Keywords[i] != keyword {
							t.Errorf("GetOperatorInfo(%v) keyword[%d] = %v, want %v", tt.token, i, info.Keywords[i], keyword)
						}
					}
				}
			}
		})
	}
}

func TestOperatorTypeChecks(t *testing.T) {
	tests := []struct {
		name    string
		token   Token
		isArith bool
		isComp  bool
		isLogic bool
	}{
		{
			name:    "Plus",
			token:   OP_PLUS,
			isArith: true,
			isComp:  false,
			isLogic: false,
		},
		{
			name:    "Equal To",
			token:   OP_EQUAL_TO,
			isArith: false,
			isComp:  true,
			isLogic: false,
		},
		{
			name:    "AND",
			token:   OP_AND,
			isArith: false,
			isComp:  false,
			isLogic: true,
		},
		{
			name:    "Not an operator",
			token:   ILLEGAL,
			isArith: false,
			isComp:  false,
			isLogic: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsArithmeticOperator(tt.token); got != tt.isArith {
				t.Errorf("IsArithmeticOperator(%v) = %v, want %v", tt.token, got, tt.isArith)
			}
			if got := IsComparisonOperator(tt.token); got != tt.isComp {
				t.Errorf("IsComparisonOperator(%v) = %v, want %v", tt.token, got, tt.isComp)
			}
			if got := IsLogicalOperator(tt.token); got != tt.isLogic {
				t.Errorf("IsLogicalOperator(%v) = %v, want %v", tt.token, got, tt.isLogic)
			}
		})
	}
}
