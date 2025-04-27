package token

import (
	"reflect"
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

func TestOperatorInfo(t *testing.T) {
	tests := []struct {
		name           string
		token          Token
		wantType       OperatorType
		wantPrecedence int
		wantUnary      bool
		wantBinary     bool
		wantSymbol     string
		wantKeywords   []string
	}{
		{
			name:           "Plus operator",
			token:          OP_PLUS,
			wantType:       OP_ARITHMETIC,
			wantPrecedence: 10,
			wantUnary:      true,
			wantBinary:     true,
			wantSymbol:     "+",
		},
		{
			name:           "Power operator",
			token:          OP_POWER,
			wantType:       OP_ARITHMETIC,
			wantPrecedence: 30,
			wantUnary:      false,
			wantBinary:     true,
			wantSymbol:     "**",
		},
		{
			name:           "Greater than operator",
			token:          OP_GREATER_THAN,
			wantType:       OP_COMPARISON,
			wantPrecedence: 5,
			wantUnary:      false,
			wantBinary:     true,
			wantSymbol:     ">",
			wantKeywords:   []string{"GREATER", "GREATER THAN"},
		},
		{
			name:           "Less equal operator",
			token:          OP_LESS_EQUAL,
			wantType:       OP_COMPARISON,
			wantPrecedence: 5,
			wantUnary:      false,
			wantBinary:     true,
			wantSymbol:     "<=",
			wantKeywords:   []string{"LESS THAN OR EQUAL", "LESS OR EQUAL"},
		},
		{
			name:           "Not equal operator",
			token:          OP_NOT_EQUAL,
			wantType:       OP_COMPARISON,
			wantPrecedence: 5,
			wantUnary:      false,
			wantBinary:     true,
			wantSymbol:     "<>",
			wantKeywords:   []string{"NOT EQUAL", "NOT EQUAL TO"},
		},
		{
			name:           "Logical NOT operator",
			token:          OP_NOT,
			wantType:       OP_LOGICAL,
			wantPrecedence: 4,
			wantUnary:      true,
			wantBinary:     false,
			wantKeywords:   []string{"NOT"},
		},
		{
			name:           "Left parenthesis",
			token:          OP_LPAREN,
			wantType:       OP_DELIMITER,
			wantPrecedence: 0,
			wantUnary:      false,
			wantBinary:     false,
			wantSymbol:     "(",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			info, ok := GetOperatorInfo(tt.token)
			if !ok {
				t.Fatalf("GetOperatorInfo(%v) returned not ok", tt.token)
			}

			if info.Type != tt.wantType {
				t.Errorf("Type = %v, want %v", info.Type, tt.wantType)
			}
			if info.Precedence != tt.wantPrecedence {
				t.Errorf("Precedence = %v, want %v", info.Precedence, tt.wantPrecedence)
			}
			if info.IsUnary != tt.wantUnary {
				t.Errorf("IsUnary = %v, want %v", info.IsUnary, tt.wantUnary)
			}
			if info.IsBinary != tt.wantBinary {
				t.Errorf("IsBinary = %v, want %v", info.IsBinary, tt.wantBinary)
			}
			if info.Symbol != tt.wantSymbol {
				t.Errorf("Symbol = %v, want %v", info.Symbol, tt.wantSymbol)
			}
			if tt.wantKeywords != nil {
				if len(info.Keywords) != len(tt.wantKeywords) {
					t.Errorf("Keywords length = %v, want %v", len(info.Keywords), len(tt.wantKeywords))
				} else {
					for i, kw := range tt.wantKeywords {
						if info.Keywords[i] != kw {
							t.Errorf("Keywords[%d] = %v, want %v", i, info.Keywords[i], kw)
						}
					}
				}
			}
		})
	}
}

func TestOperatorTypeChecks(t *testing.T) {
	tests := []struct {
		name     string
		token    Token
		checks   []func(Token) bool
		expected []bool
	}{
		{
			name:     "Plus operator type checks",
			token:    OP_PLUS,
			checks:   []func(Token) bool{IsArithmeticOperator, IsComparisonOperator, IsLogicalOperator, IsUnaryOperator, IsBinaryOperator, IsDelimiter},
			expected: []bool{true, false, false, true, true, false},
		},
		{
			name:     "Greater than operator type checks",
			token:    OP_GREATER_THAN,
			checks:   []func(Token) bool{IsArithmeticOperator, IsComparisonOperator, IsLogicalOperator, IsUnaryOperator, IsBinaryOperator, IsDelimiter},
			expected: []bool{false, true, false, false, true, false},
		},
		{
			name:     "NOT operator type checks",
			token:    OP_NOT,
			checks:   []func(Token) bool{IsArithmeticOperator, IsComparisonOperator, IsLogicalOperator, IsUnaryOperator, IsBinaryOperator, IsDelimiter},
			expected: []bool{false, false, true, true, false, false},
		},
		{
			name:     "Period delimiter type checks",
			token:    OP_PERIOD,
			checks:   []func(Token) bool{IsArithmeticOperator, IsComparisonOperator, IsLogicalOperator, IsUnaryOperator, IsBinaryOperator, IsDelimiter},
			expected: []bool{false, false, false, false, false, true},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			for i, check := range tt.checks {
				got := check(tt.token)
				if got != tt.expected[i] {
					t.Errorf("%v(%v) = %v, want %v", getFuncName(check), tt.token, got, tt.expected[i])
				}
			}
		})
	}
}

func TestOperatorPrecedence(t *testing.T) {
	tests := []struct {
		name     string
		ops      []Token
		expected []int
	}{
		{
			name:     "Arithmetic operators precedence",
			ops:      []Token{OP_PLUS, OP_MINUS, OP_MULTIPLY, OP_DIVIDE, OP_POWER},
			expected: []int{10, 10, 20, 20, 30},
		},
		{
			name:     "Comparison operators precedence",
			ops:      []Token{OP_EQUAL_TO, OP_GREATER_THAN, OP_LESS_THAN, OP_NOT_EQUAL},
			expected: []int{5, 5, 5, 5},
		},
		{
			name:     "Logical operators precedence",
			ops:      []Token{OP_AND, OP_OR, OP_NOT},
			expected: []int{3, 2, 4},
		},
		{
			name:     "Delimiters precedence",
			ops:      []Token{OP_LPAREN, OP_RPAREN, OP_PERIOD, OP_COMMA},
			expected: []int{0, 0, 0, 0},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			for i, op := range tt.ops {
				got := GetOperatorPrecedence(op)
				if got != tt.expected[i] {
					t.Errorf("GetOperatorPrecedence(%v) = %v, want %v", op, got, tt.expected[i])
				}
			}
		})
	}
}

func TestOperatorSymbolAndKeywords(t *testing.T) {
	tests := []struct {
		name        string
		token       Token
		wantSymbol  string
		wantKeyword []string
	}{
		{
			name:       "Power operator",
			token:      OP_POWER,
			wantSymbol: "**",
		},
		{
			name:        "Greater equal operator",
			token:       OP_GREATER_EQUAL,
			wantSymbol:  ">=",
			wantKeyword: []string{"GREATER THAN OR EQUAL", "GREATER OR EQUAL"},
		},
		{
			name:        "Less equal operator",
			token:       OP_LESS_EQUAL,
			wantSymbol:  "<=",
			wantKeyword: []string{"LESS THAN OR EQUAL", "LESS OR EQUAL"},
		},
		{
			name:        "Not equal operator",
			token:       OP_NOT_EQUAL,
			wantSymbol:  "<>",
			wantKeyword: []string{"NOT EQUAL", "NOT EQUAL TO"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotSymbol := GetOperatorSymbol(tt.token)
			if gotSymbol != tt.wantSymbol {
				t.Errorf("GetOperatorSymbol(%v) = %v, want %v", tt.token, gotSymbol, tt.wantSymbol)
			}

			gotKeywords := GetOperatorKeywords(tt.token)
			if tt.wantKeyword != nil {
				if len(gotKeywords) != len(tt.wantKeyword) {
					t.Errorf("GetOperatorKeywords(%v) length = %v, want %v", tt.token, len(gotKeywords), len(tt.wantKeyword))
				} else {
					for i, kw := range tt.wantKeyword {
						if gotKeywords[i] != kw {
							t.Errorf("GetOperatorKeywords(%v)[%d] = %v, want %v", tt.token, i, gotKeywords[i], kw)
						}
					}
				}
			}
		})
	}
}

// Helper function to get function name for error messages
func getFuncName(f interface{}) string {
	switch fn := f.(type) {
	case func(Token) bool:
		// Get pointer value to uniquely identify the function
		ptr := reflect.ValueOf(fn).Pointer()
		switch ptr {
		case reflect.ValueOf(IsArithmeticOperator).Pointer():
			return "IsArithmeticOperator"
		case reflect.ValueOf(IsComparisonOperator).Pointer():
			return "IsComparisonOperator"
		case reflect.ValueOf(IsLogicalOperator).Pointer():
			return "IsLogicalOperator"
		case reflect.ValueOf(IsUnaryOperator).Pointer():
			return "IsUnaryOperator"
		case reflect.ValueOf(IsBinaryOperator).Pointer():
			return "IsBinaryOperator"
		case reflect.ValueOf(IsDelimiter).Pointer():
			return "IsDelimiter"
		}
	}
	return "unknown"
}
