package token

import "fmt"

// TokenInfo contains information about a token instance
type TokenInfo struct {
	Type    Token    // The type of token
	Literal string   // The literal text of the token
	Pos     Position // The position in source code
}

// NewTokenInfo creates a new TokenInfo instance
func NewTokenInfo(typ Token, literal string, pos Position) TokenInfo {
	return TokenInfo{
		Type:    typ,
		Literal: literal,
		Pos:     pos,
	}
}

// String returns a string representation of the token info
func (t TokenInfo) String() string {
	return fmt.Sprintf("Token{Type: %s, Literal: %s, Position: %d:%d}", t.Type, t.Literal, t.Pos.Line, t.Pos.Column)
}
