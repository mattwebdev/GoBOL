package token

import "strconv"

// Token represents a COBOL token type
type Token int

// TokenType is an alias for Token to maintain compatibility
type TokenType = Token

const (
	// Special tokens
	ILLEGAL Token = iota
	EOF
	COMMENT
	IDENTIFIER
	STRING_LIT   // String literal token
	NUMBER_LIT   // Number literal token
	LEVEL_NUMBER // COBOL level number (01-49, 66, 77, 88)
)

// String returns the string representation of a token
func (t Token) String() string {
	switch t {
	case ILLEGAL:
		return "ILLEGAL"
	case EOF:
		return "EOF"
	case COMMENT:
		return "COMMENT"
	case IDENTIFIER:
		return "IDENTIFIER"
	case STRING_LIT:
		return "STRING_LIT"
	case NUMBER_LIT:
		return "NUMBER_LIT"
	case LEVEL_NUMBER:
		return "LEVEL_NUMBER"
	default:
		// For other tokens, we'll need to implement lookup tables
		// This will be handled by the token packages that define those tokens
		return "TOKEN(" + strconv.Itoa(int(t)) + ")"
	}
}
