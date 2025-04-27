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

// tokenStrings maps tokens to their string representations
var tokenStrings = map[Token]string{
	ILLEGAL:      "ILLEGAL",
	EOF:          "EOF",
	COMMENT:      "COMMENT",
	IDENTIFIER:   "IDENTIFIER",
	STRING_LIT:   "STRING_LIT",
	NUMBER_LIT:   "NUMBER_LIT",
	LEVEL_NUMBER: "LEVEL_NUMBER",
	// Verb tokens
	MOVE:         "MOVE",
	ADD:          "ADD",
	SUBTRACT:     "SUBTRACT",
	MULTIPLY:     "MULTIPLY",
	DIVIDE:       "DIVIDE",
	COMPUTE:      "COMPUTE",
	INITIALIZE:   "INITIALIZE",
	ACCEPT:       "ACCEPT",
	DISPLAY:      "DISPLAY",
	READ:         "READ",
	WRITE:        "WRITE",
	REWRITE:      "REWRITE",
	DELETE:       "DELETE",
	START:        "START",
	OPEN:         "OPEN",
	CLOSE:        "CLOSE",
	RELEASE:      "RELEASE",
	RETURN:       "RETURN",
	PERFORM:      "PERFORM",
	GO_TO:        "GO_TO",
	IF:           "IF",
	EVALUATE:     "EVALUATE",
	CONTINUE:     "CONTINUE",
	EXIT:         "EXIT",
	STOP:         "STOP",
	ALTER:        "ALTER",
	USE:          "USE",
	STRING_VERB:  "STRING",
	UNSTRING:     "UNSTRING",
	INSPECT:      "INSPECT",
	SEARCH:       "SEARCH",
	SET:          "SET",
	SORT:         "SORT",
	MERGE:        "MERGE",
	GENERATE:     "GENERATE",
	SUPPRESS:     "SUPPRESS",
	CALL:         "CALL",
	CANCEL:       "CANCEL",
	GOBACK:       "GOBACK",
	EXIT_PROGRAM: "EXIT_PROGRAM",
	// Figurative constants
	ZERO:        "ZERO",
	ZEROS:       "ZEROS",
	ZEROES:      "ZEROES",
	SPACE:       "SPACE",
	SPACES:      "SPACES",
	HIGH_VALUE:  "HIGH_VALUE",
	HIGH_VALUES: "HIGH_VALUES",
	LOW_VALUE:   "LOW_VALUE",
	LOW_VALUES:  "LOW_VALUES",
	QUOTE:       "QUOTE",
	QUOTES:      "QUOTES",
	NULL:        "NULL",
	NULLS:       "NULLS",
	ALL:         "ALL",
	// Keywords
	CORRESPONDING: "CORRESPONDING",
	CORR:          "CORR",
	WHEN:          "WHEN",
	THROUGH:       "THROUGH",
	THRU:          "THRU",
	AFTER:         "AFTER",
	BEFORE:        "BEFORE",
	STANDARD:      "STANDARD",
	BASED:         "BASED",
	BINARY:        "BINARY",
	COMPUTATIONAL: "COMPUTATIONAL",
	COMP:          "COMP",
	COMP_1:        "COMP-1",
	COMP_2:        "COMP-2",
	COMP_3:        "COMP-3",
	COMP_4:        "COMP-4",
	COMP_5:        "COMP-5",
	SYNCHRONIZED:  "SYNCHRONIZED",
	SYNC:          "SYNC",
	JUSTIFIED:     "JUSTIFIED",
	JUST:          "JUST",
	BLANK:         "BLANK",
	RENAMES:       "RENAMES",
	REDEFINES:     "REDEFINES",
	FILLER:        "FILLER",
}

// String returns the string representation of a token
func (t Token) String() string {
	if str, ok := tokenStrings[t]; ok {
		return str
	}
	return "TOKEN(" + strconv.Itoa(int(t)) + ")"
}
