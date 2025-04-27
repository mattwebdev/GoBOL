package lexer

import (
	"fmt"
	"strings"
	"unicode"

	"github.com/mattwebdev/gobol/pkg/token"
)

// Character constants
const (
	QuoteSingle = '\''
	QuoteDouble = '"'
	Space       = ' '
	Tab         = '\t'
	NewLine     = '\n'
	CarriageRet = '\r'
	Hyphen      = '-'
	Asterisk    = '*'
	GreaterThan = '>'
	EOF         = 0
)

// Debug levels
const (
	DebugNone = iota
	DebugBasic
	DebugVerbose
)

// LexerState holds the state of the lexer at a point in time
type LexerState struct {
	position     int
	readPosition int
	ch           byte
}

// Lexer represents a lexical analyzer for COBOL
type Lexer struct {
	input        string
	fixedForm    bool
	position     int  // current position in input (points to current char)
	readPosition int  // current reading position in input (after current char)
	ch           byte // current char under examination
	line         int  // current line number
	column       int  // current column in line
	debugLevel   int  // debug level for logging
}

// save captures the current state of the lexer
func (l *Lexer) save() LexerState {
	return LexerState{
		position:     l.position,
		readPosition: l.readPosition,
		ch:           l.ch,
	}
}

// restore sets the lexer state back to a previously saved state
func (l *Lexer) restore(state LexerState) {
	l.position = state.position
	l.readPosition = state.readPosition
	l.ch = state.ch
}

// New creates a new Lexer instance
func New(input string, fixedForm bool) *Lexer {
	l := &Lexer{
		input:      input,
		fixedForm:  fixedForm,
		line:       1,
		column:     1,
		debugLevel: DebugNone,
	}
	l.readChar()
	return l
}

// SetDebugLevel sets the debug level for the lexer
func (l *Lexer) SetDebugLevel(level int) {
	l.debugLevel = level
}

func (l *Lexer) debug(level int, format string, args ...interface{}) {
	if l.debugLevel >= level {
		fmt.Printf("[LEXER DEBUG] Line %d, Col %d: %s\n",
			l.line, l.column, fmt.Sprintf(format, args...))
	}
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
		// Don't increment readPosition when we hit EOF
		return
	}
	l.ch = l.input[l.readPosition]
	if l.ch == '\n' {
		l.line++
		l.column = 0 // Will be incremented to 1 below
	}
	l.column++
	l.position = l.readPosition
	l.readPosition++
}

func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}

func (l *Lexer) skipWhitespace() {
	for {
		switch l.ch {
		case Space, Tab, CarriageRet:
			l.readChar()
		case NewLine:
			l.readChar()
		default:
			return
		}
	}
}

// Optional version that can preserve newlines
func (l *Lexer) skipSpaces(preserveNewlines bool) {
	for {
		switch l.ch {
		case Space, Tab, CarriageRet:
			l.readChar()
		case NewLine:
			if preserveNewlines {
				return
			}
			l.readChar()
		default:
			return
		}
	}
}

func (l *Lexer) readIdentifier() string {
	position := l.position
	// Read the first word
	for isLetter(l.ch) || isDigit(l.ch) || l.ch == '-' || l.ch == '$' || l.ch == '(' || l.ch == ')' || l.ch == '.' {
		l.readChar()
	}
	word := strings.ToUpper(l.input[position:l.position])

	// Try to form multi-word tokens
	startState := l.save()
	l.skipWhitespace()

	if l.ch != 0 {
		// Try to read second word
		nextWordStart := l.position
		for isLetter(l.ch) || isDigit(l.ch) || l.ch == '-' || l.ch == '$' {
			l.readChar()
		}

		if l.position > nextWordStart {
			nextWord := strings.ToUpper(l.input[nextWordStart:l.position])
			twoWords := word + " " + nextWord

			// Check if two words form a valid token
			if tok := token.Lookup(twoWords); tok != token.IDENTIFIER {
				// Try to read third word
				secondState := l.save()
				l.skipWhitespace()

				if l.ch != 0 {
					thirdWordStart := l.position
					for isLetter(l.ch) || isDigit(l.ch) || l.ch == '-' || l.ch == '$' {
						l.readChar()
					}

					if l.position > thirdWordStart {
						thirdWord := strings.ToUpper(l.input[thirdWordStart:l.position])
						threeWords := twoWords + " " + thirdWord

						// Check if three words form a valid token
						if tok := token.Lookup(threeWords); tok != token.IDENTIFIER {
							return threeWords
						}
					}
				}
				// If three words didn't form a valid token, restore to two words
				l.restore(secondState)
				return twoWords
			}
		}
	}

	// If no multi-word token was found, restore to original state
	l.restore(startState)
	return word
}

func (l *Lexer) readNumber() string {
	position := l.position
	for isDigit(l.ch) || l.ch == '.' || l.ch == 'V' || l.ch == 'v' {
		l.readChar()
	}
	return l.input[position:l.position]
}

func (l *Lexer) readString() string {
	quoteChar := l.ch // Remember if it's ' or "
	position := l.position + 1
	for {
		l.readChar()
		if l.ch == quoteChar || l.ch == 0 {
			break
		}
	}
	str := l.input[position:l.position]
	l.readChar() // consume closing quote
	return str
}

func (l *Lexer) readCompilerDirective() string {
	position := l.position
	for l.ch != NewLine && l.ch != EOF {
		l.readChar()
	}
	directive := strings.TrimSpace(l.input[position:l.position])
	if l.ch == NewLine {
		l.readChar() // Consume the newline
	}
	return directive
}

// NextToken returns the next token in the input
func (l *Lexer) NextToken() token.TokenInfo {
	var tok token.Token
	var lit string

	l.skipWhitespace()

	if l.fixedForm {
		if l.column <= 6 {
			l.skipToColumn(7)
		}
		if l.column > 72 {
			l.skipToNextLine()
			return l.NextToken()
		}
	}

	startPos := token.Position{Line: l.line, Column: l.column}

	switch l.ch {
	case Asterisk:
		if l.column == 7 && l.fixedForm {
			lit = l.readCompilerDirective()
			tok = token.COMMENT
			return token.TokenInfo{Type: tok, Literal: lit, Pos: startPos}
		}
		tok = token.ASTERISK
		lit = string(l.ch)
	case '.':
		tok = token.DOT
		lit = "."
	case ',':
		tok = token.COMMA
		lit = ","
	case ';':
		tok = token.SEMICOLON
		lit = ";"
	case '(':
		if isLetter(l.peekChar()) || isDigit(l.peekChar()) {
			// This is part of a PICTURE clause or identifier
			return token.TokenInfo{
				Type:    token.IDENTIFIER,
				Literal: l.readIdentifier(),
				Pos:     startPos,
			}
		}
		tok = token.LPAREN
		lit = "("
	case ')':
		tok = token.RPAREN
		lit = ")"
	case '{':
		tok = token.LBRACE
		lit = "{"
	case '}':
		tok = token.RBRACE
		lit = "}"
	case '+':
		tok = token.PLUS
		lit = "+"
	case Hyphen:
		if l.column == 7 && l.fixedForm {
			l.readChar()
			return l.NextToken()
		}
		if isLetter(l.peekChar()) || isDigit(l.peekChar()) {
			// This is part of an identifier or hyphenated keyword
			return token.TokenInfo{
				Type:    token.IDENTIFIER,
				Literal: l.readIdentifier(),
				Pos:     startPos,
			}
		}
		tok = token.MINUS
		lit = "-"
	case '/':
		tok = token.SLASH
		lit = "/"
	case '=':
		if l.peekChar() == '=' {
			l.readChar() // consume second =
			position := l.position + 1
			for {
				l.readChar()
				if l.ch == '=' && l.peekChar() == '=' {
					break
				}
				if l.ch == EOF {
					return token.TokenInfo{
						Type:    token.ILLEGAL,
						Literal: "unclosed pseudo-text delimiter",
						Pos:     startPos,
					}
				}
			}
			lit = l.input[position:l.position]
			l.readChar() // consume first =
			l.readChar() // consume second =
			tok = token.PSEUDO_TEXT
			return token.TokenInfo{Type: tok, Literal: lit, Pos: startPos}
		}
		tok = token.EQUAL
		lit = "="
	case QuoteSingle, QuoteDouble:
		tok = token.STRING_LIT
		lit = l.readString()
	case GreaterThan:
		if l.peekChar() == GreaterThan {
			l.readChar() // consume second >
			l.readChar() // move to start of directive
			// Skip any whitespace after >>
			l.skipSpaces(true)
			// Read the directive name
			directiveStart := l.position
			for isLetter(l.ch) {
				l.readChar()
			}
			directive := strings.ToUpper(l.input[directiveStart:l.position])

			// Skip whitespace after directive name
			l.skipSpaces(true)

			// Read the rest of the line as the directive value
			valueStart := l.position
			for l.ch != NewLine && l.ch != EOF {
				l.readChar()
			}
			value := strings.TrimSpace(l.input[valueStart:l.position])

			if l.ch == NewLine {
				l.readChar() // Consume the newline
			}

			lit = directive + " " + value
			tok = token.COMPILER_DIRECTIVE
			return token.TokenInfo{Type: tok, Literal: lit, Pos: startPos}
		}
		tok = token.GREATER
		lit = ">"
	case EOF:
		tok = token.EOF
		lit = ""
	case '$':
		if isLetter(l.peekChar()) || isDigit(l.peekChar()) {
			// This is part of a PICTURE clause
			return token.TokenInfo{
				Type:    token.IDENTIFIER,
				Literal: l.readIdentifier(),
				Pos:     startPos,
			}
		}
		tok = token.ILLEGAL
		lit = fmt.Sprintf("unexpected character '%c' at line %d column %d", l.ch, l.line, l.column)
	default:
		if isLetter(l.ch) || l.ch == '_' {
			lit = l.readIdentifier()
			tok = token.Lookup(lit)
			return token.TokenInfo{Type: tok, Literal: lit, Pos: startPos}
		} else if isDigit(l.ch) {
			lit = l.readNumber()
			if token.IsLevelNumber(lit) {
				tok = token.LEVEL_NUMBER
			} else {
				tok = token.NUMERIC
			}
			return token.TokenInfo{Type: tok, Literal: lit, Pos: startPos}
		} else {
			tok = token.ILLEGAL
			lit = string(l.ch)
			l.debug(DebugBasic, "Illegal character: '%c'", l.ch)
			return token.TokenInfo{
				Type:    tok,
				Literal: fmt.Sprintf("unexpected character '%c' at line %d column %d", l.ch, l.line, l.column),
				Pos:     startPos,
			}
		}
	}

	l.readChar()
	return token.TokenInfo{Type: tok, Literal: lit, Pos: startPos}
}

func (l *Lexer) skipToColumn(col int) {
	for l.column < col && l.ch != 0 && l.ch != '\n' {
		l.readChar()
	}
}

func (l *Lexer) skipToNextLine() {
	for l.ch != 0 && l.ch != '\n' {
		l.readChar()
	}
	if l.ch == '\n' {
		l.readChar()
	}
}

func isLetter(ch byte) bool {
	return unicode.IsLetter(rune(ch)) || ch == '_'
}

func isDigit(ch byte) bool {
	return unicode.IsDigit(rune(ch))
}
