package lexer

import (
	"testing"

	"github.com/mattwebdev/gobol/pkg/token"
)

func TestNextToken(t *testing.T) {
	input := `IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GREETING PIC X(20) VALUE "Hello, World!".
       PROCEDURE DIVISION.
           DISPLAY GREETING.
           STOP RUN.`

	tests := []struct {
		expectedType    token.Token
		expectedLiteral string
	}{
		{token.IDENTIFICATION_DIVISION, "IDENTIFICATION DIVISION"},
		{token.DOT, "."},
		{token.PROGRAM_ID, "PROGRAM-ID"},
		{token.DOT, "."},
		{token.IDENT, "HELLO-WORLD"},
		{token.DOT, "."},
		{token.ENVIRONMENT_DIVISION, "ENVIRONMENT DIVISION"},
		{token.DOT, "."},
		{token.DATA_DIVISION, "DATA DIVISION"},
		{token.DOT, "."},
		{token.WORKING_STORAGE, "WORKING-STORAGE"},
		{token.SECTION, "SECTION"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "01"},
		{token.IDENT, "GREETING"},
		{token.PIC, "PIC"},
		{token.IDENT, "X(20)"},
		{token.VALUE, "VALUE"},
		{token.STRING_LIT, "Hello, World!"},
		{token.DOT, "."},
		{token.PROCEDURE_DIVISION, "PROCEDURE DIVISION"},
		{token.DOT, "."},
		{token.DISPLAY, "DISPLAY"},
		{token.IDENT, "GREETING"},
		{token.DOT, "."},
		{token.STOP, "STOP"},
		{token.IDENT, "RUN"},
		{token.DOT, "."},
		{token.EOF, ""},
	}

	l := New(input, false)

	for i, tt := range tests {
		tok := l.NextToken()

		if tok.Type != tt.expectedType {
			t.Errorf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Errorf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestSpecialTokens(t *testing.T) {
	input := `COMPUTE TOTAL = PRICE * QUANTITY + TAX.
       IF TOTAL > 1000 THEN
          MOVE SPACES TO RESULT
          MOVE ZEROS TO COUNT
       END-IF.`

	tests := []struct {
		expectedType    token.Token
		expectedLiteral string
	}{
		{token.COMPUTE, "COMPUTE"},
		{token.IDENT, "TOTAL"},
		{token.EQUAL, "="},
		{token.IDENT, "PRICE"},
		{token.ASTERISK, "*"},
		{token.IDENT, "QUANTITY"},
		{token.PLUS, "+"},
		{token.IDENT, "TAX"},
		{token.DOT, "."},
		{token.IF, "IF"},
		{token.IDENT, "TOTAL"},
		{token.GREATER, ">"},
		{token.NUMERIC, "1000"},
		{token.IDENT, "THEN"},
		{token.MOVE, "MOVE"},
		{token.SPACES, "SPACES"},
		{token.TO, "TO"},
		{token.IDENT, "RESULT"},
		{token.MOVE, "MOVE"},
		{token.ZEROS, "ZEROS"},
		{token.TO, "TO"},
		{token.IDENT, "COUNT"},
		{token.END_IF, "END-IF"},
		{token.DOT, "."},
		{token.EOF, ""},
	}

	l := New(input, false)

	for i, tt := range tests {
		tok := l.NextToken()

		if tok.Type != tt.expectedType {
			t.Errorf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Errorf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestPictureClause(t *testing.T) {
	input := `01 NUM1 PIC 9(5)V99.
       01 NUM2 PIC S9(7)V99 COMP-3.
       01 STR1 PIC X(10).
       01 EDIT1 PIC $ZZ,ZZ9.99.`

	tests := []struct {
		expectedType    token.Token
		expectedLiteral string
	}{
		{token.LEVEL_NUMBER, "01"},
		{token.IDENT, "NUM1"},
		{token.PIC, "PIC"},
		{token.IDENT, "9(5)V99"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "01"},
		{token.IDENT, "NUM2"},
		{token.PIC, "PIC"},
		{token.IDENT, "S9(7)V99"},
		{token.COMP, "COMP-3"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "01"},
		{token.IDENT, "STR1"},
		{token.PIC, "PIC"},
		{token.IDENT, "X(10)"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "01"},
		{token.IDENT, "EDIT1"},
		{token.PIC, "PIC"},
		{token.IDENT, "$ZZ,ZZ9.99"},
		{token.DOT, "."},
		{token.EOF, ""},
	}

	l := New(input, false)

	for i, tt := range tests {
		tok := l.NextToken()

		if tok.Type != tt.expectedType {
			t.Errorf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Errorf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestNextToken_FreeForm(t *testing.T) {
	input := `IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 GREETING PIC X(20) VALUE "Hello, World!".
PROCEDURE DIVISION.
    DISPLAY GREETING
    STOP RUN.`

	tests := []struct {
		expectedType    token.Token
		expectedLiteral string
	}{
		{token.IDENTIFICATION_DIVISION, "IDENTIFICATION"},
		{token.DOT, "."},
		{token.PROGRAM_ID, "PROGRAM-ID"},
		{token.DOT, "."},
		{token.IDENT, "HELLO-WORLD"},
		{token.DOT, "."},
		{token.ENVIRONMENT_DIVISION, "ENVIRONMENT"},
		{token.DOT, "."},
		{token.DATA_DIVISION, "DATA"},
		{token.DOT, "."},
		{token.WORKING_STORAGE, "WORKING-STORAGE"},
		{token.SECTION, "SECTION"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "01"},
		{token.IDENT, "GREETING"},
		{token.PIC, "PIC"},
		{token.IDENT, "X"},
		{token.LPAREN, "("},
		{token.NUMERIC, "20"},
		{token.RPAREN, ")"},
		{token.VALUE, "VALUE"},
		{token.STRING_LIT, "\"Hello, World!\""},
		{token.DOT, "."},
		{token.PROCEDURE_DIVISION, "PROCEDURE"},
		{token.DOT, "."},
		{token.DISPLAY, "DISPLAY"},
		{token.IDENT, "GREETING"},
		{token.STOP, "STOP"},
		{token.IDENT, "RUN"},
		{token.DOT, "."},
		{token.EOF, ""},
	}

	l := New(input, false)

	for i, tt := range tests {
		tok := l.NextToken()
		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestNextToken_FixedForm(t *testing.T) {
	input := `000100 IDENTIFICATION DIVISION.                                               
000200 PROGRAM-ID. HELLO-WORLD.                                              
000300 ENVIRONMENT DIVISION.                                                 
000400 DATA DIVISION.                                                        
000500 WORKING-STORAGE SECTION.                                             
000600 01 GREETING PIC X(20) VALUE "Hello, World!".                         
000700 PROCEDURE DIVISION.                                                   
000800     DISPLAY GREETING                                                  
000900     STOP RUN.                                                        `

	tests := []struct {
		expectedType    token.Token
		expectedLiteral string
	}{
		{token.IDENTIFICATION_DIVISION, "IDENTIFICATION"},
		{token.DOT, "."},
		{token.PROGRAM_ID, "PROGRAM-ID"},
		{token.DOT, "."},
		{token.IDENT, "HELLO-WORLD"},
		{token.DOT, "."},
		{token.ENVIRONMENT_DIVISION, "ENVIRONMENT"},
		{token.DOT, "."},
		{token.DATA_DIVISION, "DATA"},
		{token.DOT, "."},
		{token.WORKING_STORAGE, "WORKING-STORAGE"},
		{token.SECTION, "SECTION"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "01"},
		{token.IDENT, "GREETING"},
		{token.PIC, "PIC"},
		{token.IDENT, "X"},
		{token.LPAREN, "("},
		{token.NUMERIC, "20"},
		{token.RPAREN, ")"},
		{token.VALUE, "VALUE"},
		{token.STRING_LIT, "\"Hello, World!\""},
		{token.DOT, "."},
		{token.PROCEDURE_DIVISION, "PROCEDURE"},
		{token.DOT, "."},
		{token.DISPLAY, "DISPLAY"},
		{token.IDENT, "GREETING"},
		{token.STOP, "STOP"},
		{token.IDENT, "RUN"},
		{token.DOT, "."},
		{token.EOF, ""},
	}

	l := New(input, true)

	for i, tt := range tests {
		tok := l.NextToken()
		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestCommentAndDebugLines(t *testing.T) {
	input := `000100 IDENTIFICATION DIVISION.                                               
000200* THIS IS A COMMENT                                                    
000300 PROGRAM-ID. HELLO-WORLD.                                              
000400D    DISPLAY "DEBUG LINE"                                              
000500 ENVIRONMENT DIVISION.                                                 `

	l := New(input, true)

	expectedTokens := []struct {
		Type    token.Token
		Literal string
	}{
		{token.IDENTIFICATION_DIVISION, "IDENTIFICATION"},
		{token.DOT, "."},
		{token.COMMENT, "* THIS IS A COMMENT"},
		{token.PROGRAM_ID, "PROGRAM-ID"},
		{token.DOT, "."},
		{token.IDENT, "HELLO-WORLD"},
		{token.DOT, "."},
		{token.ENVIRONMENT_DIVISION, "ENVIRONMENT"},
		{token.DOT, "."},
		{token.EOF, ""},
	}

	for i, expected := range expectedTokens {
		tok := l.NextToken()
		if tok.Type != expected.Type {
			t.Errorf("tests[%d] - wrong token type. expected=%q, got=%q",
				i, expected.Type, tok.Type)
		}
	}
}

func TestCompoundTokens(t *testing.T) {
	input := `IF A IS GREATER THAN B
   IF C IS LESS THAN OR EQUAL TO D
      MOVE LENGTH OF MyVar TO LenVar
      SET ADDRESS OF Buffer TO MemAddr
      MOVE HIGH-VALUE TO MyVar
      MOVE LOW-VALUES TO OtherVar`

	tests := []struct {
		expectedType    token.Token
		expectedLiteral string
	}{
		{token.IF, "IF"},
		{token.IDENT, "A"},
		{token.IS, "IS"},
		{token.GREATER_THAN, "GREATER THAN"},
		{token.IDENT, "B"},
		{token.IF, "IF"},
		{token.IDENT, "C"},
		{token.IS, "IS"},
		{token.LESS, "LESS THAN OR"},
		{token.EQUAL_TO, "EQUAL TO"},
		{token.IDENT, "D"},
		{token.MOVE, "MOVE"},
		{token.LENGTH_OF, "LENGTH OF"},
		{token.IDENT, "MyVar"},
		{token.TO, "TO"},
		{token.IDENT, "LenVar"},
		{token.SET, "SET"},
		{token.ADDRESS_OF, "ADDRESS OF"},
		{token.IDENT, "Buffer"},
		{token.TO, "TO"},
		{token.IDENT, "MemAddr"},
		{token.MOVE, "MOVE"},
		{token.HIGH_VALUE, "HIGH-VALUE"},
		{token.TO, "TO"},
		{token.IDENT, "MyVar"},
		{token.MOVE, "MOVE"},
		{token.LOW_VALUES, "LOW-VALUES"},
		{token.TO, "TO"},
		{token.IDENT, "OtherVar"},
	}

	l := New(input, false)

	for i, tt := range tests {
		tok := l.NextToken()
		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestLevelNumbers(t *testing.T) {
	input := `01  GROUP-ITEM.
    05  SUBITEM-1   PIC X(10).
    05  SUBITEM-2   PIC 9(5).
    10  NESTED-1    PIC X(5).
    10  NESTED-2    PIC X(5).
66  RENAMES-ITEM RENAMES SUBITEM-1 THRU NESTED-2.
77  WORKING-STORAGE-ITEM PIC X(20).
88  VALID-VALUES VALUE "A" "B" "C".`

	tests := []struct {
		expectedType    token.Token
		expectedLiteral string
	}{
		{token.LEVEL_NUMBER, "01"},
		{token.IDENT, "GROUP-ITEM"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "05"},
		{token.IDENT, "SUBITEM-1"},
		{token.PIC, "PIC"},
		{token.IDENT, "X"},
		{token.LPAREN, "("},
		{token.NUMERIC, "10"},
		{token.RPAREN, ")"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "05"},
		{token.IDENT, "SUBITEM-2"},
		{token.PIC, "PIC"},
		{token.NUMERIC, "9"},
		{token.LPAREN, "("},
		{token.NUMERIC, "5"},
		{token.RPAREN, ")"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "10"},
		{token.IDENT, "NESTED-1"},
		{token.PIC, "PIC"},
		{token.IDENT, "X"},
		{token.LPAREN, "("},
		{token.NUMERIC, "5"},
		{token.RPAREN, ")"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "10"},
		{token.IDENT, "NESTED-2"},
		{token.PIC, "PIC"},
		{token.IDENT, "X"},
		{token.LPAREN, "("},
		{token.NUMERIC, "5"},
		{token.RPAREN, ")"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "66"},
		{token.IDENT, "RENAMES-ITEM"},
		{token.REDEFINES, "RENAMES"},
		{token.IDENT, "SUBITEM-1"},
		{token.THRU, "THRU"},
		{token.IDENT, "NESTED-2"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "77"},
		{token.IDENT, "WORKING-STORAGE-ITEM"},
		{token.PIC, "PIC"},
		{token.IDENT, "X"},
		{token.LPAREN, "("},
		{token.NUMERIC, "20"},
		{token.RPAREN, ")"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "88"},
		{token.IDENT, "VALID-VALUES"},
		{token.VALUE, "VALUE"},
		{token.STRING_LIT, "\"A\""},
		{token.STRING_LIT, "\"B\""},
		{token.STRING_LIT, "\"C\""},
		{token.DOT, "."},
	}

	l := New(input, false)

	for i, tt := range tests {
		tok := l.NextToken()
		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}
