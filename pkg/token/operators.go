package token

// Operator tokens start at 500
const (
	// Arithmetic operators
	OP_PLUS Token = iota + 500
	OP_MINUS
	OP_MULTIPLY
	OP_DIVIDE
	OP_POWER

	// Comparison operators
	OP_EQUAL_TO
	OP_GREATER_THAN
	OP_LESS_THAN
	OP_GREATER_EQUAL
	OP_LESS_EQUAL
	OP_NOT_EQUAL

	// Logical operators
	OP_AND
	OP_OR
	OP_NOT

	// Parentheses and delimiters
	OP_LPAREN // (
	OP_RPAREN // )
	OP_PERIOD // .
	OP_COMMA  // ,
)

// OperatorType represents the type of operator
type OperatorType int

const (
	OP_UNKNOWN OperatorType = iota
	OP_ARITHMETIC
	OP_COMPARISON
	OP_LOGICAL
	OP_DELIMITER
)

// OperatorInfo contains information about an operator
type OperatorInfo struct {
	Token      Token
	Type       OperatorType
	Precedence int      // Operator precedence (higher = evaluated first)
	IsUnary    bool     // Can be used as unary operator
	IsBinary   bool     // Can be used as binary operator
	Symbol     string   // The symbol representation
	Keywords   []string // Keyword representations (e.g., "EQUAL TO")
}

// Operator definitions
var operatorInfo = map[Token]OperatorInfo{
	OP_PLUS: {
		Token:      OP_PLUS,
		Type:       OP_ARITHMETIC,
		Precedence: 10,
		IsUnary:    true,
		IsBinary:   true,
		Symbol:     "+",
	},
	OP_MINUS: {
		Token:      OP_MINUS,
		Type:       OP_ARITHMETIC,
		Precedence: 10,
		IsUnary:    true,
		IsBinary:   true,
		Symbol:     "-",
	},
	OP_MULTIPLY: {
		Token:      OP_MULTIPLY,
		Type:       OP_ARITHMETIC,
		Precedence: 20,
		IsBinary:   true,
		Symbol:     "*",
	},
	OP_DIVIDE: {
		Token:      OP_DIVIDE,
		Type:       OP_ARITHMETIC,
		Precedence: 20,
		IsBinary:   true,
		Symbol:     "/",
	},
	OP_EQUAL_TO: {
		Token:      OP_EQUAL_TO,
		Type:       OP_COMPARISON,
		Precedence: 5,
		IsBinary:   true,
		Symbol:     "=",
		Keywords:   []string{"EQUAL", "EQUAL TO", "EQUALS"},
	},
	OP_GREATER_THAN: {
		Token:      OP_GREATER_THAN,
		Type:       OP_COMPARISON,
		Precedence: 5,
		IsBinary:   true,
		Symbol:     ">",
		Keywords:   []string{"GREATER", "GREATER THAN"},
	},
	OP_AND: {
		Token:      OP_AND,
		Type:       OP_LOGICAL,
		Precedence: 3,
		IsBinary:   true,
		Keywords:   []string{"AND"},
	},
	OP_OR: {
		Token:      OP_OR,
		Type:       OP_LOGICAL,
		Precedence: 2,
		IsBinary:   true,
		Keywords:   []string{"OR"},
	},
}

// IsOperator checks if a token is an operator
func IsOperator(t Token) bool {
	_, ok := operatorInfo[t]
	return ok
}

// GetOperatorInfo returns information about an operator
func GetOperatorInfo(t Token) (OperatorInfo, bool) {
	info, ok := operatorInfo[t]
	return info, ok
}

// IsArithmeticOperator checks if a token is an arithmetic operator
func IsArithmeticOperator(t Token) bool {
	if info, ok := operatorInfo[t]; ok {
		return info.Type == OP_ARITHMETIC
	}
	return false
}

// IsComparisonOperator checks if a token is a comparison operator
func IsComparisonOperator(t Token) bool {
	if info, ok := operatorInfo[t]; ok {
		return info.Type == OP_COMPARISON
	}
	return false
}

// IsLogicalOperator checks if a token is a logical operator
func IsLogicalOperator(t Token) bool {
	if info, ok := operatorInfo[t]; ok {
		return info.Type == OP_LOGICAL
	}
	return false
}
