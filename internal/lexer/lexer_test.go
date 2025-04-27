package lexer

import (
	"testing"

	"github.com/mattwebdev/gobol/pkg/token"
)

// TestBasicTokens tests the lexer's ability to recognize basic COBOL tokens
func TestBasicTokens(t *testing.T) {
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
		{token.IDENTIFIER, "HELLO-WORLD"},
		{token.DOT, "."},
		{token.ENVIRONMENT_DIVISION, "ENVIRONMENT DIVISION"},
		{token.DOT, "."},
		{token.DATA_DIVISION, "DATA DIVISION"},
		{token.DOT, "."},
		{token.WORKING_STORAGE_SECTION, "WORKING-STORAGE SECTION"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "01"},
		{token.IDENTIFIER, "GREETING"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "X(20)"},
		{token.VALUE, "VALUE"},
		{token.STRING_LIT, "Hello, World!"},
		{token.DOT, "."},
		{token.PROCEDURE_DIVISION, "PROCEDURE DIVISION"},
		{token.DOT, "."},
		{token.DISPLAY, "DISPLAY"},
		{token.IDENTIFIER, "GREETING"},
		{token.DOT, "."},
		{token.STOP, "STOP"},
		{token.IDENTIFIER, "RUN"},
		{token.DOT, "."},
		{token.EOF, ""},
	}

	runTokenTest(t, input, tests)
}

// TestFixedFormCOBOL tests the lexer's handling of fixed-form COBOL format
func TestFixedFormCOBOL(t *testing.T) {
	input := `000100 IDENTIFICATION DIVISION.                                         
000200 PROGRAM-ID. FIXED-FORM.                                          
000300* THIS IS A COMMENT                                              
000400 ENVIRONMENT DIVISION.                                           
000500 DATA DIVISION.                                                  
000600 WORKING-STORAGE SECTION.                                        
000700 01  CUSTOMER-RECORD.                                            
000800     05  CUST-NAME    PIC X(20).                                
000900     05  CUST-ID      PIC 9(5).                                 
001000     05  CUST-BAL     PIC S9(7)V99   COMP-3.                   
001100 PROCEDURE DIVISION.                                             
001200     DISPLAY "FIXED FORM TEST".                                  
001300     STOP RUN.                                                  `

	tests := []struct {
		expectedType    token.Token
		expectedLiteral string
	}{
		{token.IDENTIFICATION_DIVISION, "IDENTIFICATION DIVISION"},
		{token.DOT, "."},
		{token.PROGRAM_ID, "PROGRAM-ID"},
		{token.DOT, "."},
		{token.IDENTIFIER, "FIXED-FORM"},
		{token.DOT, "."},
		{token.COMMENT, "* THIS IS A COMMENT"},
		{token.ENVIRONMENT_DIVISION, "ENVIRONMENT DIVISION"},
		{token.DOT, "."},
		{token.DATA_DIVISION, "DATA DIVISION"},
		{token.DOT, "."},
		{token.WORKING_STORAGE_SECTION, "WORKING-STORAGE SECTION"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "01"},
		{token.IDENTIFIER, "CUSTOMER-RECORD"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "05"},
		{token.IDENTIFIER, "CUST-NAME"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "X(20)"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "05"},
		{token.IDENTIFIER, "CUST-ID"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "9(5)"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "05"},
		{token.IDENTIFIER, "CUST-BAL"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "S9(7)V99"},
		{token.COMP, "COMP-3"},
		{token.DOT, "."},
		{token.PROCEDURE_DIVISION, "PROCEDURE DIVISION"},
		{token.DOT, "."},
		{token.DISPLAY, "DISPLAY"},
		{token.STRING_LIT, "FIXED FORM TEST"},
		{token.DOT, "."},
		{token.STOP, "STOP"},
		{token.IDENTIFIER, "RUN"},
		{token.DOT, "."},
		{token.EOF, ""},
	}

	runTokenTest(t, input, tests, true) // true indicates fixed-form
}

// TestPictureClausesAndDataTypes tests various PICTURE clause formats and data types
func TestPictureClausesAndDataTypes(t *testing.T) {
	input := `       01  DATA-ITEMS.
           05  NUM1        PIC 9(5)V99.
           05  NUM2        PIC S9(7)V99     COMP-3.
           05  NUM3        PIC PPP999       VALUE 1000.
           05  STR1        PIC X(10).
           05  EDIT1       PIC $ZZ,ZZ9.99.
           05  EDIT2       PIC ++++9.99.
           05  EDIT3       PIC ----9.99.
           05  FLOAT1      PIC 9(5)V9(2)E+99.
           05  BOOL1       PIC 1            VALUE B"1".
           05  HEX1        PIC X(4)         VALUE H"FFFF".
           05  NATIONAL    PIC N(10).`

	tests := []struct {
		expectedType    token.Token
		expectedLiteral string
	}{
		{token.LEVEL_NUMBER, "01"},
		{token.IDENTIFIER, "DATA-ITEMS"},
		{token.DOT, "."},
		// NUM1
		{token.LEVEL_NUMBER, "05"},
		{token.IDENTIFIER, "NUM1"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "9(5)V99"},
		{token.DOT, "."},
		// NUM2
		{token.LEVEL_NUMBER, "05"},
		{token.IDENTIFIER, "NUM2"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "S9(7)V99"},
		{token.COMP, "COMP-3"},
		{token.DOT, "."},
		// NUM3
		{token.LEVEL_NUMBER, "05"},
		{token.IDENTIFIER, "NUM3"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "PPP999"},
		{token.VALUE, "VALUE"},
		{token.NUMERIC, "1000"},
		{token.DOT, "."},
		// STR1
		{token.LEVEL_NUMBER, "05"},
		{token.IDENTIFIER, "STR1"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "X(10)"},
		{token.DOT, "."},
		// EDIT1
		{token.LEVEL_NUMBER, "05"},
		{token.IDENTIFIER, "EDIT1"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "$ZZ,ZZ9.99"},
		{token.DOT, "."},
		// EDIT2
		{token.LEVEL_NUMBER, "05"},
		{token.IDENTIFIER, "EDIT2"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "++++9.99"},
		{token.DOT, "."},
		// EDIT3
		{token.LEVEL_NUMBER, "05"},
		{token.IDENTIFIER, "EDIT3"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "----9.99"},
		{token.DOT, "."},
		// FLOAT1
		{token.LEVEL_NUMBER, "05"},
		{token.IDENTIFIER, "FLOAT1"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "9(5)V9(2)E+99"},
		{token.DOT, "."},
		// BOOL1
		{token.LEVEL_NUMBER, "05"},
		{token.IDENTIFIER, "BOOL1"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "1"},
		{token.VALUE, "VALUE"},
		{token.IDENTIFIER, "B\"1\""},
		{token.DOT, "."},
		// HEX1
		{token.LEVEL_NUMBER, "05"},
		{token.IDENTIFIER, "HEX1"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "X(4)"},
		{token.VALUE, "VALUE"},
		{token.IDENTIFIER, "H\"FFFF\""},
		{token.DOT, "."},
		// NATIONAL
		{token.LEVEL_NUMBER, "05"},
		{token.IDENTIFIER, "NATIONAL"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "N(10)"},
		{token.DOT, "."},
	}

	runTokenTest(t, input, tests)
}

// TestSpecialRegistersAndFigurativeConstants tests COBOL's built-in constants and registers
func TestSpecialRegistersAndFigurativeConstants(t *testing.T) {
	input := `       MOVE LENGTH OF CUSTOMER-RECORD TO LEN-VAR
           MOVE ADDRESS OF BUFFER TO PTR-VAR
           MOVE ZERO         TO NUM1
           MOVE ZEROS        TO NUM2
           MOVE ZEROES      TO NUM3
           MOVE SPACE       TO STR1
           MOVE SPACES      TO STR2
           MOVE HIGH-VALUE  TO FLAG1
           MOVE HIGH-VALUES TO FLAG2
           MOVE LOW-VALUE   TO FLAG3
           MOVE LOW-VALUES  TO FLAG4
           MOVE ALL "*"     TO FILLER1
           MOVE ALL SPACES  TO FILLER2
           MOVE QUOTE       TO DELIM1
           MOVE QUOTES      TO DELIM2
           MOVE NULL        TO PTR1
           MOVE NULLS       TO PTR2.`

	tests := []struct {
		expectedType    token.Token
		expectedLiteral string
	}{
		{token.MOVE, "MOVE"},
		{token.LENGTH_OF, "LENGTH OF"},
		{token.IDENTIFIER, "CUSTOMER-RECORD"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "LEN-VAR"},
		{token.MOVE, "MOVE"},
		{token.ADDRESS_OF, "ADDRESS OF"},
		{token.IDENTIFIER, "BUFFER"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "PTR-VAR"},
		{token.MOVE, "MOVE"},
		{token.ZERO, "ZERO"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "NUM1"},
		{token.MOVE, "MOVE"},
		{token.ZEROS, "ZEROS"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "NUM2"},
		{token.MOVE, "MOVE"},
		{token.ZEROES, "ZEROES"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "NUM3"},
		{token.MOVE, "MOVE"},
		{token.SPACE, "SPACE"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "STR1"},
		{token.MOVE, "MOVE"},
		{token.SPACES, "SPACES"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "STR2"},
		{token.MOVE, "MOVE"},
		{token.HIGH_VALUE, "HIGH-VALUE"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "FLAG1"},
		{token.MOVE, "MOVE"},
		{token.HIGH_VALUES, "HIGH-VALUES"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "FLAG2"},
		{token.MOVE, "MOVE"},
		{token.LOW_VALUE, "LOW-VALUE"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "FLAG3"},
		{token.MOVE, "MOVE"},
		{token.LOW_VALUES, "LOW-VALUES"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "FLAG4"},
		{token.MOVE, "MOVE"},
		{token.ALL, "ALL"},
		{token.STRING_LIT, "*"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "FILLER1"},
		{token.MOVE, "MOVE"},
		{token.ALL, "ALL"},
		{token.SPACES, "SPACES"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "FILLER2"},
		{token.MOVE, "MOVE"},
		{token.QUOTE, "QUOTE"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "DELIM1"},
		{token.MOVE, "MOVE"},
		{token.QUOTES, "QUOTES"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "DELIM2"},
		{token.MOVE, "MOVE"},
		{token.NULL, "NULL"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "PTR1"},
		{token.MOVE, "MOVE"},
		{token.NULLS, "NULLS"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "PTR2"},
		{token.DOT, "."},
	}

	runTokenTest(t, input, tests)
}

// TestFixedFormContinuation tests continuation lines in fixed-form COBOL
func TestFixedFormContinuation(t *testing.T) {
	input := `000100 77  LONG-STRING            PIC X(100)        VALUE              
000200-    "This is a very long string that needs to be continued on the 
000300-    "next line because it won't fit on a single line in fixed-form
000400-    "COBOL format.".                                              
000500 77  CUSTOMER-NAME          PIC X(50)         VALUE "JOHN DOE".   
000600 77  PHONE-NUMBER          PIC X(20)         VALUE               
000700-    "(555) 123-4567".                                           `

	tests := []struct {
		expectedType    token.Token
		expectedLiteral string
	}{
		{token.LEVEL_NUMBER, "77"},
		{token.IDENTIFIER, "LONG-STRING"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "X(100)"},
		{token.VALUE, "VALUE"},
		{token.STRING_LIT, "This is a very long string that needs to be continued on the next line because it won't fit on a single line in fixed-form COBOL format."},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "77"},
		{token.IDENTIFIER, "CUSTOMER-NAME"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "X(50)"},
		{token.VALUE, "VALUE"},
		{token.STRING_LIT, "JOHN DOE"},
		{token.DOT, "."},
		{token.LEVEL_NUMBER, "77"},
		{token.IDENTIFIER, "PHONE-NUMBER"},
		{token.PIC, "PIC"},
		{token.IDENTIFIER, "X(20)"},
		{token.VALUE, "VALUE"},
		{token.STRING_LIT, "(555) 123-4567"},
		{token.DOT, "."},
	}

	runTokenTest(t, input, tests, true) // true indicates fixed-form
}

// TestCompilerDirectivesAndCopyStatements tests compiler directives and COPY statements
func TestCompilerDirectivesAndCopyStatements(t *testing.T) {
	input := `>>SOURCE FORMAT IS FIXED
000100 COPY "CUSTOMER.cpy".
>>SOURCE FORMAT IS FREE
       COPY "ORDERS.cpy"
         REPLACING ==CUSTOMER-ID== BY ==ORDER-CUST-ID==
                   ==AMOUNT== BY ==ORDER-AMOUNT==.
>>IF CICS
       EXEC CICS READ
           DATASET('CUSTFILE')
           INTO(CUSTOMER-RECORD)
           RIDFLD(CUST-ID)
           RESP(WS-RESP)
       END-EXEC
>>ELSE
       READ CUSTFILE
           INTO CUSTOMER-RECORD
           KEY IS CUST-ID
           INVALID KEY
               MOVE "NOT FOUND" TO STATUS-MESSAGE
       END-READ
>>END-IF`

	tests := []struct {
		expectedType    token.Token
		expectedLiteral string
	}{
		{token.COMPILER_DIRECTIVE, ">>SOURCE FORMAT IS FIXED"},
		{token.COPY, "COPY"},
		{token.STRING_LIT, "CUSTOMER.cpy"},
		{token.DOT, "."},
		{token.COMPILER_DIRECTIVE, ">>SOURCE FORMAT IS FREE"},
		{token.COPY, "COPY"},
		{token.STRING_LIT, "ORDERS.cpy"},
		{token.REPLACING, "REPLACING"},
		{token.PSEUDO_TEXT, "=="},
		{token.IDENTIFIER, "CUSTOMER-ID"},
		{token.PSEUDO_TEXT, "=="},
		{token.BY, "BY"},
		{token.PSEUDO_TEXT, "=="},
		{token.IDENTIFIER, "ORDER-CUST-ID"},
		{token.PSEUDO_TEXT, "=="},
		{token.PSEUDO_TEXT, "=="},
		{token.IDENTIFIER, "AMOUNT"},
		{token.PSEUDO_TEXT, "=="},
		{token.BY, "BY"},
		{token.PSEUDO_TEXT, "=="},
		{token.IDENTIFIER, "ORDER-AMOUNT"},
		{token.PSEUDO_TEXT, "=="},
		{token.DOT, "."},
		{token.COMPILER_DIRECTIVE, ">>IF CICS"},
		{token.EXEC_CICS, "EXEC CICS"},
		{token.IDENTIFIER, "READ"},
		{token.IDENTIFIER, "DATASET"},
		{token.LPAREN, "("},
		{token.STRING_LIT, "CUSTFILE"},
		{token.RPAREN, ")"},
		{token.IDENTIFIER, "INTO"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "CUSTOMER-RECORD"},
		{token.RPAREN, ")"},
		{token.IDENTIFIER, "RIDFLD"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "CUST-ID"},
		{token.RPAREN, ")"},
		{token.IDENTIFIER, "RESP"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "WS-RESP"},
		{token.RPAREN, ")"},
		{token.END_EXEC, "END-EXEC"},
		{token.COMPILER_DIRECTIVE, ">>ELSE"},
		{token.READ, "READ"},
		{token.IDENTIFIER, "CUSTFILE"},
		{token.INTO, "INTO"},
		{token.IDENTIFIER, "CUSTOMER-RECORD"},
		{token.KEY, "KEY"},
		{token.IS, "IS"},
		{token.IDENTIFIER, "CUST-ID"},
		{token.INVALID_KEY, "INVALID KEY"},
		{token.MOVE, "MOVE"},
		{token.STRING_LIT, "NOT FOUND"},
		{token.TO, "TO"},
		{token.IDENTIFIER, "STATUS-MESSAGE"},
		{token.END_READ, "END-READ"},
		{token.COMPILER_DIRECTIVE, ">>END-IF"},
	}

	runTokenTest(t, input, tests)
}

// TestFileOperations tests file handling operations in COBOL
func TestFileOperations(t *testing.T) {
	input := `       SELECT CUSTOMER-FILE
           ASSIGN TO "CUSTFILE"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CUSTOMER-ID
           ALTERNATE RECORD KEY IS CUSTOMER-NAME WITH DUPLICATES
           FILE STATUS IS FILE-STATUS.

       FD  CUSTOMER-FILE
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 5 RECORDS
           RECORDING MODE IS F.

           OPEN INPUT CUSTOMER-FILE
           READ CUSTOMER-FILE
               KEY IS CUSTOMER-ID
               INVALID KEY
                   PERFORM CUSTOMER-NOT-FOUND
               NOT INVALID KEY
                   PERFORM PROCESS-CUSTOMER
           END-READ
           CLOSE CUSTOMER-FILE.

           OPEN OUTPUT NEW-FILE
           WRITE CUSTOMER-RECORD
               INVALID KEY
                   DISPLAY "Write failed"
           END-WRITE
           CLOSE NEW-FILE.

           OPEN I-O UPDATE-FILE
           REWRITE CUSTOMER-RECORD
               INVALID KEY
                   PERFORM REWRITE-ERROR
           END-REWRITE
           CLOSE UPDATE-FILE.

           DELETE CUSTOMER-FILE RECORD
               INVALID KEY
                   PERFORM DELETE-ERROR
           END-DELETE.`

	tests := []struct {
		expectedType    token.Token
		expectedLiteral string
	}{
		{token.SELECT, "SELECT"},
		{token.IDENTIFIER, "CUSTOMER-FILE"},
		{token.ASSIGN, "ASSIGN"},
		{token.TO, "TO"},
		{token.STRING_LIT, "CUSTFILE"},
		{token.ORGANIZATION, "ORGANIZATION"},
		{token.IS, "IS"},
		{token.INDEXED, "INDEXED"},
		{token.ACCESS, "ACCESS"},
		{token.MODE, "MODE"},
		{token.IS, "IS"},
		{token.DYNAMIC, "DYNAMIC"},
		{token.RECORD, "RECORD"},
		{token.KEY, "KEY"},
		{token.IS, "IS"},
		{token.IDENTIFIER, "CUSTOMER-ID"},
		{token.ALTERNATE, "ALTERNATE"},
		{token.RECORD, "RECORD"},
		{token.KEY, "KEY"},
		{token.IS, "IS"},
		{token.IDENTIFIER, "CUSTOMER-NAME"},
		{token.WITH, "WITH"},
		{token.DUPLICATES, "DUPLICATES"},
		{token.FILE_STATUS, "FILE STATUS"},
		{token.IS, "IS"},
		{token.IDENTIFIER, "FILE-STATUS"},
		{token.DOT, "."},
		{token.FD, "FD"},
		{token.IDENTIFIER, "CUSTOMER-FILE"},
		{token.LABEL, "LABEL"},
		{token.RECORDS, "RECORDS"},
		{token.ARE, "ARE"},
		{token.STANDARD, "STANDARD"},
		{token.BLOCK, "BLOCK"},
		{token.CONTAINS, "CONTAINS"},
		{token.NUMERIC, "5"},
		{token.RECORDS, "RECORDS"},
		{token.RECORDING, "RECORDING"},
		{token.MODE, "MODE"},
		{token.IS, "IS"},
		{token.IDENTIFIER, "F"},
		{token.DOT, "."},
		{token.OPEN, "OPEN"},
		{token.INPUT, "INPUT"},
		{token.IDENTIFIER, "CUSTOMER-FILE"},
		{token.READ, "READ"},
		{token.IDENTIFIER, "CUSTOMER-FILE"},
		{token.KEY, "KEY"},
		{token.IS, "IS"},
		{token.IDENTIFIER, "CUSTOMER-ID"},
		{token.INVALID_KEY, "INVALID KEY"},
		{token.PERFORM, "PERFORM"},
		{token.IDENTIFIER, "CUSTOMER-NOT-FOUND"},
		{token.NOT, "NOT"},
		{token.INVALID_KEY, "INVALID KEY"},
		{token.PERFORM, "PERFORM"},
		{token.IDENTIFIER, "PROCESS-CUSTOMER"},
		{token.END_READ, "END-READ"},
		{token.CLOSE, "CLOSE"},
		{token.IDENTIFIER, "CUSTOMER-FILE"},
		{token.DOT, "."},
		{token.OPEN, "OPEN"},
		{token.OUTPUT, "OUTPUT"},
		{token.IDENTIFIER, "NEW-FILE"},
		{token.WRITE, "WRITE"},
		{token.IDENTIFIER, "CUSTOMER-RECORD"},
		{token.INVALID_KEY, "INVALID KEY"},
		{token.DISPLAY, "DISPLAY"},
		{token.STRING_LIT, "Write failed"},
		{token.END_WRITE, "END-WRITE"},
		{token.CLOSE, "CLOSE"},
		{token.IDENTIFIER, "NEW-FILE"},
		{token.DOT, "."},
		{token.OPEN, "OPEN"},
		{token.I_O, "I-O"},
		{token.IDENTIFIER, "UPDATE-FILE"},
		{token.REWRITE, "REWRITE"},
		{token.IDENTIFIER, "CUSTOMER-RECORD"},
		{token.INVALID_KEY, "INVALID KEY"},
		{token.PERFORM, "PERFORM"},
		{token.IDENTIFIER, "REWRITE-ERROR"},
		{token.END_REWRITE, "END-REWRITE"},
		{token.CLOSE, "CLOSE"},
		{token.IDENTIFIER, "UPDATE-FILE"},
		{token.DOT, "."},
		{token.DELETE, "DELETE"},
		{token.IDENTIFIER, "CUSTOMER-FILE"},
		{token.RECORD, "RECORD"},
		{token.INVALID_KEY, "INVALID KEY"},
		{token.PERFORM, "PERFORM"},
		{token.IDENTIFIER, "DELETE-ERROR"},
		{token.END_DELETE, "END-DELETE"},
		{token.DOT, "."},
	}

	runTokenTest(t, input, tests)
}

// Helper function to run token tests
func runTokenTest(t *testing.T, input string, tests []struct {
	expectedType    token.Token
	expectedLiteral string
}, isFixedForm ...bool) {
	fixedForm := false
	if len(isFixedForm) > 0 {
		fixedForm = isFixedForm[0]
	}

	l := New(input, fixedForm)

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
