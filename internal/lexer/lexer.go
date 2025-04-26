package lexer

import (
	"strings"
	"unicode"

	"github.com/mattwebdev/gobol/pkg/token"
)

// Lexer represents a lexical analyzer for COBOL
type Lexer struct {
	input        string
	position     int  // current position in input (points to current char)
	readPosition int  // current reading position in input (after current char)
	ch           byte // current char under examination
	line         int  // current line number
	column       int  // current column number
	isFixedForm  bool // whether we're parsing fixed-form COBOL
}

// New creates a new Lexer instance
func New(input string, isFixedForm bool) *Lexer {
	l := &Lexer{
		input:       input,
		isFixedForm: isFixedForm,
		line:        1,
		column:      0,
	}
	l.readChar()
	return l
}

// readChar reads the next character and advances the position in the input string
func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition++
	l.column++
}

// NextToken returns the next token from the input
func (l *Lexer) NextToken() token.TokenInfo {
	var tok token.TokenInfo

	l.skipWhitespace()

	// Handle fixed-form COBOL specific rules
	if l.isFixedForm && l.column == 1 {
		return l.handleFixedFormLine()
	}

	switch l.ch {
	case '*':
		if l.column == 7 && l.isFixedForm {
			return l.readComment()
		}
		tok = l.newToken(token.ASTERISK, string(l.ch))
	case '.':
		tok = l.newToken(token.DOT, string(l.ch))
	case ',':
		tok = l.newToken(token.COMMA, string(l.ch))
	case ';':
		tok = l.newToken(token.SEMICOLON, string(l.ch))
	case '(':
		tok = l.newToken(token.LPAREN, string(l.ch))
	case ')':
		tok = l.newToken(token.RPAREN, string(l.ch))
	case '{':
		tok = l.newToken(token.LBRACE, string(l.ch))
	case '}':
		tok = l.newToken(token.RBRACE, string(l.ch))
	case '+':
		tok = l.newToken(token.PLUS, string(l.ch))
	case '-':
		// Handle COBOL words with hyphens
		if isLetter(l.peekChar()) {
			tok = l.readCompoundIdentifier()
			return tok
		} else {
			tok = l.newToken(token.MINUS, string(l.ch))
		}
	case '/':
		tok = l.newToken(token.SLASH, string(l.ch))
	case '=':
		tok = l.newToken(token.EQUAL, string(l.ch))
	case '"', '\'':
		tok = l.readString(l.ch)
	case '$':
		tok = l.newToken(token.CURRENCY, string(l.ch))
	case 0:
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if isLetter(l.ch) {
			tok = l.readCompoundIdentifier()
			return tok
		} else if isDigit(l.ch) {
			return l.readNumber()
		} else {
			tok = l.newToken(token.ILLEGAL, string(l.ch))
		}
	}

	l.readChar()
	return tok
}

// readCompoundIdentifier reads an identifier that might be part of a compound token
func (l *Lexer) readCompoundIdentifier() token.TokenInfo {
	pos := l.currentPosition()
	word := l.readWord()

	// Check for compound tokens (e.g., "GREATER THAN", "LENGTH OF")
	if isCompoundTokenStart(word) {
		l.skipWhitespace()
		if isLetter(l.ch) {
			nextWord := l.readWord()
			compound := word + " " + nextWord

			// Handle three-word compounds (e.g., "GREATER THAN OR")
			if isThreeWordCompoundStart(compound) {
				l.skipWhitespace()
				if isLetter(l.ch) {
					thirdWord := l.readWord()
					compound = compound + " " + thirdWord
				}
			}

			if typ, ok := lookupCompoundToken(compound); ok {
				return token.TokenInfo{
					Type:    typ,
					Literal: compound,
					Pos:     pos,
				}
			}
		}
	}

	// Not a compound token, treat as regular identifier
	typ := token.Lookup(word)
	return token.TokenInfo{
		Type:    typ,
		Literal: word,
		Pos:     pos,
	}
}

// readWord reads a single word (for compound token handling)
func (l *Lexer) readWord() string {
	position := l.position
	for isLetter(l.ch) || isDigit(l.ch) || l.ch == '-' {
		l.readChar()
	}
	return l.input[position:l.position]
}

// isCompoundTokenStart checks if a word could be the start of a compound token
func isCompoundTokenStart(word string) bool {
	switch strings.ToUpper(word) {
	case "GREATER", "LESS", "EQUAL", "LENGTH", "ADDRESS", "HIGH", "LOW":
		return true
	}
	return false
}

// isThreeWordCompoundStart checks if a two-word compound could start a three-word compound
func isThreeWordCompoundStart(compound string) bool {
	switch strings.ToUpper(compound) {
	case "GREATER THAN", "LESS THAN":
		return true
	}
	return false
}

// lookupCompoundToken checks if a multi-word string is a valid compound token
func lookupCompoundToken(compound string) (token.Token, bool) {
	switch strings.ToUpper(compound) {
	case "GREATER THAN":
		return token.GREATER_THAN, true
	case "LESS THAN":
		return token.LESS_THAN, true
	case "EQUAL TO":
		return token.EQUAL_TO, true
	case "GREATER THAN OR":
		return token.GREATER, true
	case "LESS THAN OR":
		return token.LESS, true
	case "LENGTH OF":
		return token.LENGTH_OF, true
	case "ADDRESS OF":
		return token.ADDRESS_OF, true
	case "HIGH VALUE":
		return token.HIGH_VALUE, true
	case "HIGH VALUES":
		return token.HIGH_VALUES, true
	case "LOW VALUE":
		return token.LOW_VALUE, true
	case "LOW VALUES":
		return token.LOW_VALUES, true
	}
	return token.ILLEGAL, false
}

// handleFixedFormLine handles the special rules for fixed-form COBOL
func (l *Lexer) handleFixedFormLine() token.TokenInfo {
	// Columns 1-6 are sequence number area
	// Column 7 is indicator area
	// Columns 8-72 are program text area
	// Columns 73-80 are identification area

	// Skip sequence number area (columns 1-6)
	for i := 1; i <= 6; i++ {
		l.readChar()
	}

	// Check indicator area (column 7)
	switch l.ch {
	case '*', '/':
		return l.readComment()
	case '-':
		// Continuation line
		l.readChar() // Skip the continuation character
		return l.NextToken()
	case 'D', 'd':
		// Debug line
		return l.readDebugLine()
	}

	// Regular program text
	l.readChar() // Move to column 8
	return l.NextToken()
}

// readComment reads a comment until the end of the line
func (l *Lexer) readComment() token.TokenInfo {
	pos := l.currentPosition()
	for l.ch != '\n' && l.ch != 0 {
		l.readChar()
	}
	return token.TokenInfo{
		Type:    token.COMMENT,
		Literal: l.input[pos.Offset:l.position],
		Pos:     pos,
	}
}

// readDebugLine reads a debug line (only valid in fixed-form)
func (l *Lexer) readDebugLine() token.TokenInfo {
	// Skip until end of line or EOF
	for l.ch != '\n' && l.ch != 0 {
		l.readChar()
	}
	return l.NextToken()
}

// readString reads a string literal
func (l *Lexer) readString(quote byte) token.TokenInfo {
	pos := l.currentPosition()
	l.readChar() // Skip opening quote
	for l.ch != quote && l.ch != 0 {
		l.readChar()
	}
	if l.ch == quote {
		l.readChar() // Skip closing quote
	}
	return token.TokenInfo{
		Type:    token.STRING_LIT,
		Literal: l.input[pos.Offset:l.position],
		Pos:     pos,
	}
}

// readNumber reads a numeric literal
func (l *Lexer) readNumber() token.TokenInfo {
	pos := l.currentPosition()
	for isDigit(l.ch) {
		l.readChar()
	}
	if l.ch == '.' && isDigit(l.peekChar()) {
		l.readChar() // consume the dot
		for isDigit(l.ch) {
			l.readChar()
		}
	}

	// Check if this could be a level number
	literal := l.input[pos.Offset:l.position]
	if token.IsLevelNumber(literal) {
		return token.TokenInfo{
			Type:    token.LEVEL_NUMBER,
			Literal: literal,
			Pos:     pos,
		}
	}

	return token.TokenInfo{
		Type:    token.NUMERIC,
		Literal: literal,
		Pos:     pos,
	}
}

// skipWhitespace skips any whitespace characters
func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		if l.ch == '\n' {
			l.line++
			l.column = 0
		}
		l.readChar()
	}
}

// peekChar returns the next character without advancing the position
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

// currentPosition returns the current position information
func (l *Lexer) currentPosition() token.Position {
	return token.Position{
		Line:   l.line,
		Column: l.column,
		Offset: l.position,
	}
}

// newToken creates a new token with the current position information
func (l *Lexer) newToken(tokenType token.Token, literal string) token.TokenInfo {
	return token.TokenInfo{
		Type:    tokenType,
		Literal: literal,
		Pos:     l.currentPosition(),
	}
}

// Helper functions
func isLetter(ch byte) bool {
	return unicode.IsLetter(rune(ch)) || ch == '_'
}

func isDigit(ch byte) bool {
	return unicode.IsDigit(rune(ch))
}
