package token

import (
	"testing"
)

func TestKeywordTokenValues(t *testing.T) {
	// Test that keyword tokens start at 600
	if GO < 600 {
		t.Errorf("Expected keyword tokens to start at 600, but GO token is %d", GO)
	}

	// Test that all keyword tokens are unique
	seen := make(map[Token]string)
	tokens := []struct {
		tok  Token
		name string
	}{
		{GO, "GO"},
		{TO, "TO"},
		{THAN, "THAN"},
		{OR, "OR"},
		{AND, "AND"},
		{IS, "IS"},
		{NOT, "NOT"},
		{IN, "IN"},
		{BY, "BY"},
		{FROM, "FROM"},
		{GIVING, "GIVING"},
		{WHEN, "WHEN"},
		{THROUGH, "THROUGH"},
		{THRU, "THRU"},
		{AFTER, "AFTER"},
		{BEFORE, "BEFORE"},
		{STANDARD, "STANDARD"},
		{IDENTIFICATION, "IDENTIFICATION"},
		{ENVIRONMENT, "ENVIRONMENT"},
		{DATA, "DATA"},
		{PROCEDURE, "PROCEDURE"},
		{DIVISION, "DIVISION"},
		{SECTION, "SECTION"},
		{PICTURE, "PICTURE"},
		{PIC, "PIC"},
		{USAGE, "USAGE"},
		{VALUE, "VALUE"},
		{VALUES, "VALUES"},
		{OCCURS, "OCCURS"},
		{TIMES, "TIMES"},
		{DEPENDING, "DEPENDING"},
		{ON, "ON"},
		{INDEXED, "INDEXED"},
		{VARYING, "VARYING"},
		{UNTIL, "UNTIL"},
		{BINARY, "BINARY"},
		{COMPUTATIONAL, "COMPUTATIONAL"},
		{COMP, "COMP"},
		{COMP_1, "COMP_1"},
		{COMP_2, "COMP_2"},
		{COMP_3, "COMP_3"},
		{COMP_4, "COMP_4"},
		{COMP_5, "COMP_5"},
		{SYNCHRONIZED, "SYNCHRONIZED"},
		{SYNC, "SYNC"},
		{JUSTIFIED, "JUSTIFIED"},
		{JUST, "JUST"},
		{BLANK, "BLANK"},
		{RENAMES, "RENAMES"},
		{REDEFINES, "REDEFINES"},
		{FILLER, "FILLER"},
		{GREATER, "GREATER"},
		{LESS, "LESS"},
		{EQUAL, "EQUAL"},
		{EQUALS, "EQUALS"},
		{CORRESPONDING, "CORRESPONDING"},
		{CORR, "CORR"},
		{BASED, "BASED"},
		{END_IF, "END-IF"},
		{END_READ, "END-READ"},
		{END_WRITE, "END-WRITE"},
		{END_PERFORM, "END-PERFORM"},
		{END_EVALUATE, "END-EVALUATE"},
		{END_SEARCH, "END-SEARCH"},
		{END_COMPUTE, "END-COMPUTE"},
		{END_ADD, "END-ADD"},
		{END_SUBTRACT, "END-SUBTRACT"},
		{END_MULTIPLY, "END-MULTIPLY"},
		{END_DIVIDE, "END-DIVIDE"},
		{END_STRING, "END-STRING"},
		{END_UNSTRING, "END-UNSTRING"},
		{END_CALL, "END-CALL"},
		{END_ACCEPT, "END-ACCEPT"},
		{END_DISPLAY, "END-DISPLAY"},
		{ORGANIZATION, "ORGANIZATION"},
		{SEQUENTIAL, "SEQUENTIAL"},
		{RELATIVE, "RELATIVE"},
		{ACCESS, "ACCESS"},
		{RANDOM, "RANDOM"},
		{DYNAMIC, "DYNAMIC"},
		{FILE, "FILE"},
		{STATUS, "STATUS"},
		{RECORD, "RECORD"},
		{KEY, "KEY"},
		{ALTERNATE, "ALTERNATE"},
		{LABEL, "LABEL"},
		{BLOCK, "BLOCK"},
		{CONTAINS, "CONTAINS"},
		{RECORDING, "RECORDING"},
		{MODE, "MODE"},
		{LINE, "LINE"},
		{ADVANCING, "ADVANCING"},
		{OPTIONAL, "OPTIONAL"},
		{LINAGE, "LINAGE"},
		{FOOTING, "FOOTING"},
		{TOP, "TOP"},
		{BOTTOM, "BOTTOM"},
		{PADDING, "PADDING"},
		{CHARACTER, "CHARACTER"},
		{DATA_RECORD, "DATA_RECORD"},
		{CODE_SET, "CODE_SET"},
		{SHARING, "SHARING"},
		{WITH, "WITH"},
		{NO, "NO"},
		{OTHER, "OTHER"},
		{LOCK, "LOCK"},
		{AUTOMATIC, "AUTOMATIC"},
		{MANUAL, "MANUAL"},
		{EXCLUSIVE, "EXCLUSIVE"},
		{RETRY, "RETRY"},
		{REPORT, "REPORT"},
		{PAGE, "PAGE"},
		{HEADING, "HEADING"},
		{FIRST, "FIRST"},
		{LAST, "LAST"},
		{DETAIL, "DETAIL"},
		{CONTROL, "CONTROL"},
		{FINAL, "FINAL"},
		{SUM, "SUM"},
		{RESET, "RESET"},
	}

	for _, tt := range tokens {
		if existing, exists := seen[tt.tok]; exists {
			t.Errorf("Token value collision between %s and %s", tt.name, existing)
		}
		seen[tt.tok] = tt.name
	}
}

func TestIsKeyword(t *testing.T) {
	tests := []struct {
		name     string
		token    Token
		expected bool
	}{
		{name: "CORRESPONDING", token: CORRESPONDING, expected: true},
		{name: "TO", token: TO, expected: true},
		{name: "GIVING", token: GIVING, expected: true},
		{name: "GREATER", token: GREATER, expected: true},
		{name: "BINARY", token: BINARY, expected: true},
		{name: "COMPUTATIONAL", token: COMPUTATIONAL, expected: true},
		{name: "COMP", token: COMP, expected: true},
		{name: "SYNCHRONIZED", token: SYNCHRONIZED, expected: true},
		{name: "SYNC", token: SYNC, expected: true},
		{name: "JUSTIFIED", token: JUSTIFIED, expected: true},
		{name: "JUST", token: JUST, expected: true},
		{name: "BLANK", token: BLANK, expected: true},
		{name: "RENAMES", token: RENAMES, expected: true},
		{name: "REDEFINES", token: REDEFINES, expected: true},
		{name: "FILLER", token: FILLER, expected: true},
		{name: "THROUGH", token: THROUGH, expected: true},
		{name: "THRU", token: THRU, expected: true},
		{name: "WHEN", token: WHEN, expected: true},
		{name: "AFTER", token: AFTER, expected: true},
		{name: "BEFORE", token: BEFORE, expected: true},
		{name: "STANDARD", token: STANDARD, expected: true},
		{name: "BASED", token: BASED, expected: true},
		{name: "ORGANIZATION", token: ORGANIZATION, expected: true},
		{name: "SEQUENTIAL", token: SEQUENTIAL, expected: true},
		{name: "RELATIVE", token: RELATIVE, expected: true},
		{name: "ACCESS", token: ACCESS, expected: true},
		{name: "RANDOM", token: RANDOM, expected: true},
		{name: "DYNAMIC", token: DYNAMIC, expected: true},
		{name: "FILE", token: FILE, expected: true},
		{name: "STATUS", token: STATUS, expected: true},
		{name: "RECORD", token: RECORD, expected: true},
		{name: "KEY", token: KEY, expected: true},
		{name: "ALTERNATE", token: ALTERNATE, expected: true},
		{name: "LABEL", token: LABEL, expected: true},
		{name: "BLOCK", token: BLOCK, expected: true},
		{name: "CONTAINS", token: CONTAINS, expected: true},
		{name: "RECORDING", token: RECORDING, expected: true},
		{name: "MODE", token: MODE, expected: true},
		{name: "ILLEGAL", token: ILLEGAL, expected: false},
		{name: "EOF", token: EOF, expected: false},
		{name: "IDENTIFIER", token: IDENTIFIER, expected: false},
		{name: "END_IF is keyword", token: END_IF, expected: true},
		{name: "END_READ is keyword", token: END_READ, expected: true},
		{name: "END_WRITE is keyword", token: END_WRITE, expected: true},
		{name: "END_PERFORM is keyword", token: END_PERFORM, expected: true},
		{name: "END_EVALUATE is keyword", token: END_EVALUATE, expected: true},
		{name: "END_SEARCH is keyword", token: END_SEARCH, expected: true},
		{name: "END_COMPUTE is keyword", token: END_COMPUTE, expected: true},
		{name: "END_ADD is keyword", token: END_ADD, expected: true},
		{name: "END_SUBTRACT is keyword", token: END_SUBTRACT, expected: true},
		{name: "END_MULTIPLY is keyword", token: END_MULTIPLY, expected: true},
		{name: "END_DIVIDE is keyword", token: END_DIVIDE, expected: true},
		{name: "END_STRING is keyword", token: END_STRING, expected: true},
		{name: "END_UNSTRING is keyword", token: END_UNSTRING, expected: true},
		{name: "END_CALL is keyword", token: END_CALL, expected: true},
		{name: "END_ACCEPT is keyword", token: END_ACCEPT, expected: true},
		{name: "END_DISPLAY is keyword", token: END_DISPLAY, expected: true},
		{name: "LINE", token: LINE, expected: true},
		{name: "ADVANCING", token: ADVANCING, expected: true},
		{name: "OPTIONAL", token: OPTIONAL, expected: true},
		{name: "LINAGE", token: LINAGE, expected: true},
		{name: "FOOTING", token: FOOTING, expected: true},
		{name: "TOP", token: TOP, expected: true},
		{name: "BOTTOM", token: BOTTOM, expected: true},
		{name: "PADDING", token: PADDING, expected: true},
		{name: "CHARACTER", token: CHARACTER, expected: true},
		{name: "DATA_RECORD", token: DATA_RECORD, expected: true},
		{name: "CODE_SET", token: CODE_SET, expected: true},
		{name: "SHARING", token: SHARING, expected: true},
		{name: "WITH", token: WITH, expected: true},
		{name: "NO", token: NO, expected: true},
		{name: "OTHER", token: OTHER, expected: true},
		{name: "LOCK", token: LOCK, expected: true},
		{name: "AUTOMATIC", token: AUTOMATIC, expected: true},
		{name: "MANUAL", token: MANUAL, expected: true},
		{name: "EXCLUSIVE", token: EXCLUSIVE, expected: true},
		{name: "RETRY", token: RETRY, expected: true},
		{name: "REPORT", token: REPORT, expected: true},
		{name: "PAGE", token: PAGE, expected: true},
		{name: "HEADING", token: HEADING, expected: true},
		{name: "FIRST", token: FIRST, expected: true},
		{name: "LAST", token: LAST, expected: true},
		{name: "DETAIL", token: DETAIL, expected: true},
		{name: "CONTROL", token: CONTROL, expected: true},
		{name: "FINAL", token: FINAL, expected: true},
		{name: "SUM", token: SUM, expected: true},
		{name: "RESET", token: RESET, expected: true},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsKeyword(tt.token); got != tt.expected {
				t.Errorf("IsKeyword(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestGetKeywordInfo(t *testing.T) {
	tests := []struct {
		name     string
		token    Token
		wantInfo KeywordInfo
		wantOk   bool
	}{
		{
			name:  "CORRESPONDING info",
			token: CORRESPONDING,
			wantInfo: KeywordInfo{
				Token:    CORRESPONDING,
				Class:    CLASS_MODIFIER,
				Aliases:  []string{"CORR"},
				Category: "modifier",
			},
			wantOk: true,
		},
		{
			name:  "COMPUTATIONAL info",
			token: COMPUTATIONAL,
			wantInfo: KeywordInfo{
				Token:    COMPUTATIONAL,
				Class:    CLASS_KEYWORD,
				Aliases:  []string{"COMP"},
				Category: "usage",
			},
			wantOk: true,
		},
		{
			name:  "SYNCHRONIZED info",
			token: SYNCHRONIZED,
			wantInfo: KeywordInfo{
				Token:    SYNCHRONIZED,
				Class:    CLASS_KEYWORD,
				Aliases:  []string{"SYNC"},
				Category: "alignment",
			},
			wantOk: true,
		},
		{
			name:  "JUSTIFIED info",
			token: JUSTIFIED,
			wantInfo: KeywordInfo{
				Token:    JUSTIFIED,
				Class:    CLASS_KEYWORD,
				Aliases:  []string{"JUST"},
				Category: "alignment",
			},
			wantOk: true,
		},
		{
			name:  "THROUGH info",
			token: THROUGH,
			wantInfo: KeywordInfo{
				Token:    THROUGH,
				Class:    CLASS_KEYWORD,
				Aliases:  []string{"THRU"},
				Category: "range",
			},
			wantOk: true,
		},
		{
			name:  "WHEN info",
			token: WHEN,
			wantInfo: KeywordInfo{
				Token:    WHEN,
				Class:    CLASS_KEYWORD,
				Category: "control",
			},
			wantOk: true,
		},
		{
			name:  "AFTER info",
			token: AFTER,
			wantInfo: KeywordInfo{
				Token:    AFTER,
				Class:    CLASS_KEYWORD,
				Category: "position",
			},
			wantOk: true,
		},
		{
			name:  "BEFORE info",
			token: BEFORE,
			wantInfo: KeywordInfo{
				Token:    BEFORE,
				Class:    CLASS_KEYWORD,
				Category: "position",
			},
			wantOk: true,
		},
		{
			name:  "STANDARD info",
			token: STANDARD,
			wantInfo: KeywordInfo{
				Token:    STANDARD,
				Class:    CLASS_KEYWORD,
				Category: "qualifier",
			},
			wantOk: true,
		},
		{
			name:  "BASED info",
			token: BASED,
			wantInfo: KeywordInfo{
				Token:    BASED,
				Class:    CLASS_KEYWORD,
				Category: "memory",
			},
			wantOk: true,
		},
		{
			name:  "ORGANIZATION info",
			token: ORGANIZATION,
			wantInfo: KeywordInfo{
				Token:    ORGANIZATION,
				Class:    CLASS_KEYWORD,
				Category: "file_description",
			},
			wantOk: true,
		},
		{
			name:  "SEQUENTIAL info",
			token: SEQUENTIAL,
			wantInfo: KeywordInfo{
				Token:    SEQUENTIAL,
				Class:    CLASS_KEYWORD,
				Category: "file_organization",
			},
			wantOk: true,
		},
		{
			name:  "RELATIVE info",
			token: RELATIVE,
			wantInfo: KeywordInfo{
				Token:    RELATIVE,
				Class:    CLASS_KEYWORD,
				Category: "file_organization",
			},
			wantOk: true,
		},
		{
			name:  "ACCESS info",
			token: ACCESS,
			wantInfo: KeywordInfo{
				Token:    ACCESS,
				Class:    CLASS_KEYWORD,
				Category: "file_description",
			},
			wantOk: true,
		},
		{
			name:  "RANDOM info",
			token: RANDOM,
			wantInfo: KeywordInfo{
				Token:    RANDOM,
				Class:    CLASS_KEYWORD,
				Category: "access_mode",
			},
			wantOk: true,
		},
		{
			name:  "DYNAMIC info",
			token: DYNAMIC,
			wantInfo: KeywordInfo{
				Token:    DYNAMIC,
				Class:    CLASS_KEYWORD,
				Category: "access_mode",
			},
			wantOk: true,
		},
		{
			name:  "FILE info",
			token: FILE,
			wantInfo: KeywordInfo{
				Token:    FILE,
				Class:    CLASS_KEYWORD,
				Category: "file",
			},
			wantOk: true,
		},
		{
			name:  "STATUS info",
			token: STATUS,
			wantInfo: KeywordInfo{
				Token:    STATUS,
				Class:    CLASS_KEYWORD,
				Category: "file_description",
			},
			wantOk: true,
		},
		{
			name:  "RECORD info",
			token: RECORD,
			wantInfo: KeywordInfo{
				Token:    RECORD,
				Class:    CLASS_KEYWORD,
				Category: "record",
			},
			wantOk: true,
		},
		{
			name:  "KEY info",
			token: KEY,
			wantInfo: KeywordInfo{
				Token:    KEY,
				Class:    CLASS_KEYWORD,
				Category: "record_description",
			},
			wantOk: true,
		},
		{
			name:  "ALTERNATE info",
			token: ALTERNATE,
			wantInfo: KeywordInfo{
				Token:    ALTERNATE,
				Class:    CLASS_KEYWORD,
				Category: "record_description",
			},
			wantOk: true,
		},
		{
			name:  "LABEL info",
			token: LABEL,
			wantInfo: KeywordInfo{
				Token:    LABEL,
				Class:    CLASS_KEYWORD,
				Category: "file_description",
			},
			wantOk: true,
		},
		{
			name:  "BLOCK info",
			token: BLOCK,
			wantInfo: KeywordInfo{
				Token:    BLOCK,
				Class:    CLASS_KEYWORD,
				Category: "file_description",
			},
			wantOk: true,
		},
		{
			name:  "CONTAINS info",
			token: CONTAINS,
			wantInfo: KeywordInfo{
				Token:    CONTAINS,
				Class:    CLASS_KEYWORD,
				Category: "file_description",
			},
			wantOk: true,
		},
		{
			name:  "RECORDING info",
			token: RECORDING,
			wantInfo: KeywordInfo{
				Token:    RECORDING,
				Class:    CLASS_KEYWORD,
				Category: "file_description",
			},
			wantOk: true,
		},
		{
			name:  "MODE info",
			token: MODE,
			wantInfo: KeywordInfo{
				Token:    MODE,
				Class:    CLASS_KEYWORD,
				Category: "file_description",
			},
			wantOk: true,
		},
		{
			name:  "END_IF info",
			token: END_IF,
			wantInfo: KeywordInfo{
				Token:    END_IF,
				Class:    CLASS_KEYWORD,
				Context:  []Token{IF},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_READ info",
			token: END_READ,
			wantInfo: KeywordInfo{
				Token:    END_READ,
				Class:    CLASS_KEYWORD,
				Context:  []Token{READ},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_WRITE info",
			token: END_WRITE,
			wantInfo: KeywordInfo{
				Token:    END_WRITE,
				Class:    CLASS_KEYWORD,
				Context:  []Token{WRITE},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_PERFORM info",
			token: END_PERFORM,
			wantInfo: KeywordInfo{
				Token:    END_PERFORM,
				Class:    CLASS_KEYWORD,
				Context:  []Token{PERFORM},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_EVALUATE info",
			token: END_EVALUATE,
			wantInfo: KeywordInfo{
				Token:    END_EVALUATE,
				Class:    CLASS_KEYWORD,
				Context:  []Token{EVALUATE},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_SEARCH info",
			token: END_SEARCH,
			wantInfo: KeywordInfo{
				Token:    END_SEARCH,
				Class:    CLASS_KEYWORD,
				Context:  []Token{SEARCH},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_COMPUTE info",
			token: END_COMPUTE,
			wantInfo: KeywordInfo{
				Token:    END_COMPUTE,
				Class:    CLASS_KEYWORD,
				Context:  []Token{COMPUTE},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_ADD info",
			token: END_ADD,
			wantInfo: KeywordInfo{
				Token:    END_ADD,
				Class:    CLASS_KEYWORD,
				Context:  []Token{ADD},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_SUBTRACT info",
			token: END_SUBTRACT,
			wantInfo: KeywordInfo{
				Token:    END_SUBTRACT,
				Class:    CLASS_KEYWORD,
				Context:  []Token{SUBTRACT},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_MULTIPLY info",
			token: END_MULTIPLY,
			wantInfo: KeywordInfo{
				Token:    END_MULTIPLY,
				Class:    CLASS_KEYWORD,
				Context:  []Token{MULTIPLY},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_DIVIDE info",
			token: END_DIVIDE,
			wantInfo: KeywordInfo{
				Token:    END_DIVIDE,
				Class:    CLASS_KEYWORD,
				Context:  []Token{DIVIDE},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_STRING info",
			token: END_STRING,
			wantInfo: KeywordInfo{
				Token:    END_STRING,
				Class:    CLASS_KEYWORD,
				Context:  []Token{STRING_VERB},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_UNSTRING info",
			token: END_UNSTRING,
			wantInfo: KeywordInfo{
				Token:    END_UNSTRING,
				Class:    CLASS_KEYWORD,
				Context:  []Token{UNSTRING},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_CALL info",
			token: END_CALL,
			wantInfo: KeywordInfo{
				Token:    END_CALL,
				Class:    CLASS_KEYWORD,
				Context:  []Token{CALL},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_ACCEPT info",
			token: END_ACCEPT,
			wantInfo: KeywordInfo{
				Token:    END_ACCEPT,
				Class:    CLASS_KEYWORD,
				Context:  []Token{ACCEPT},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:  "END_DISPLAY info",
			token: END_DISPLAY,
			wantInfo: KeywordInfo{
				Token:    END_DISPLAY,
				Class:    CLASS_KEYWORD,
				Context:  []Token{DISPLAY},
				Category: "scope_terminator",
			},
			wantOk: true,
		},
		{
			name:     "SIGN",
			token:    SIGN,
			wantInfo: KeywordInfo{Token: SIGN, Class: CLASS_KEYWORD, Category: "data_description"},
			wantOk:   true,
		},
		{
			name:     "SEPARATE",
			token:    SEPARATE,
			wantInfo: KeywordInfo{Token: SEPARATE, Class: CLASS_KEYWORD, Category: "data_description"},
			wantOk:   true,
		},
		{
			name:     "PACKED-DECIMAL",
			token:    PACKED_DECIMAL,
			wantInfo: KeywordInfo{Token: PACKED_DECIMAL, Class: CLASS_KEYWORD, Category: "data_description"},
			wantOk:   true,
		},
		{
			name:     "DISPLAY-1",
			token:    DISPLAY_1,
			wantInfo: KeywordInfo{Token: DISPLAY_1, Class: CLASS_KEYWORD, Category: "data_description"},
			wantOk:   true,
		},
		{
			name:     "GLOBAL",
			token:    GLOBAL,
			wantInfo: KeywordInfo{Token: GLOBAL, Class: CLASS_KEYWORD, Category: "data_description"},
			wantOk:   true,
		},
		{
			name:     "EXTERNAL",
			token:    EXTERNAL,
			wantInfo: KeywordInfo{Token: EXTERNAL, Class: CLASS_KEYWORD, Category: "data_description"},
			wantOk:   true,
		},
		{
			name:     "NUMERIC",
			token:    NUMERIC,
			wantInfo: KeywordInfo{Token: NUMERIC, Class: CLASS_KEYWORD, Category: "condition"},
			wantOk:   true,
		},
		{
			name:     "ALPHABETIC",
			token:    ALPHABETIC,
			wantInfo: KeywordInfo{Token: ALPHABETIC, Class: CLASS_KEYWORD, Category: "condition"},
			wantOk:   true,
		},
		{
			name:     "ALPHABETIC-LOWER",
			token:    ALPHABETIC_LOWER,
			wantInfo: KeywordInfo{Token: ALPHABETIC_LOWER, Class: CLASS_KEYWORD, Category: "condition"},
			wantOk:   true,
		},
		{
			name:     "ALPHABETIC-UPPER",
			token:    ALPHABETIC_UPPER,
			wantInfo: KeywordInfo{Token: ALPHABETIC_UPPER, Class: CLASS_KEYWORD, Category: "condition"},
			wantOk:   true,
		},
		{
			name:     "CLASS",
			token:    CLASS,
			wantInfo: KeywordInfo{Token: CLASS, Class: CLASS_KEYWORD, Category: "condition"},
			wantOk:   true,
		},
		{
			name:     "POSITIVE",
			token:    POSITIVE,
			wantInfo: KeywordInfo{Token: POSITIVE, Class: CLASS_KEYWORD, Category: "condition"},
			wantOk:   true,
		},
		{
			name:     "NEGATIVE",
			token:    NEGATIVE,
			wantInfo: KeywordInfo{Token: NEGATIVE, Class: CLASS_KEYWORD, Category: "condition"},
			wantOk:   true,
		},
		{
			name:     "DECIMAL-POINT",
			token:    DECIMAL_POINT,
			wantInfo: KeywordInfo{Token: DECIMAL_POINT, Class: CLASS_KEYWORD, Category: "environment"},
			wantOk:   true,
		},
		{
			name:     "CURRENCY",
			token:    CURRENCY,
			wantInfo: KeywordInfo{Token: CURRENCY, Class: CLASS_KEYWORD, Category: "environment"},
			wantOk:   true,
		},
		{
			name:     "CONSOLE",
			token:    CONSOLE,
			wantInfo: KeywordInfo{Token: CONSOLE, Class: CLASS_KEYWORD, Category: "environment"},
			wantOk:   true,
		},
		{
			name:     "PRINTER",
			token:    PRINTER,
			wantInfo: KeywordInfo{Token: PRINTER, Class: CLASS_KEYWORD, Category: "environment"},
			wantOk:   true,
		},
		{
			name:     "SYSIN",
			token:    SYSIN,
			wantInfo: KeywordInfo{Token: SYSIN, Class: CLASS_KEYWORD, Category: "environment"},
			wantOk:   true,
		},
		{
			name:     "SYSOUT",
			token:    SYSOUT,
			wantInfo: KeywordInfo{Token: SYSOUT, Class: CLASS_KEYWORD, Category: "environment"},
			wantOk:   true,
		},
		{
			name:     "USING",
			token:    USING,
			wantInfo: KeywordInfo{Token: USING, Class: CLASS_KEYWORD, Category: "procedure"},
			wantOk:   true,
		},
		{
			name:     "RETURNING",
			token:    RETURNING,
			wantInfo: KeywordInfo{Token: RETURNING, Class: CLASS_KEYWORD, Category: "procedure"},
			wantOk:   true,
		},
		{
			name:     "RAISING",
			token:    RAISING,
			wantInfo: KeywordInfo{Token: RAISING, Class: CLASS_KEYWORD, Category: "procedure"},
			wantOk:   true,
		},
		{
			name:     "EXCEPTION",
			token:    EXCEPTION,
			wantInfo: KeywordInfo{Token: EXCEPTION, Class: CLASS_KEYWORD, Category: "condition"},
			wantOk:   true,
		},
		{
			name:     "SIZE",
			token:    SIZE,
			wantInfo: KeywordInfo{Token: SIZE, Class: CLASS_KEYWORD, Category: "procedure"},
			wantOk:   true,
		},
		{
			name:     "ERROR",
			token:    ERROR,
			wantInfo: KeywordInfo{Token: ERROR, Class: CLASS_KEYWORD, Category: "procedure"},
			wantOk:   true,
		},
		{
			name:     "OVERFLOW",
			token:    OVERFLOW,
			wantInfo: KeywordInfo{Token: OVERFLOW, Class: CLASS_KEYWORD, Category: "procedure"},
			wantOk:   true,
		},
		{
			name:     "UNDERFLOW",
			token:    UNDERFLOW,
			wantInfo: KeywordInfo{Token: UNDERFLOW, Class: CLASS_KEYWORD, Category: "procedure"},
			wantOk:   true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			info, exists := GetKeywordInfo(tt.token)
			if exists != tt.wantOk {
				t.Errorf("GetKeywordInfo(%v) exists = %v, want %v", tt.token, exists, tt.wantOk)
				return
			}
			if !exists {
				return
			}
			if info.Class != tt.wantInfo.Class {
				t.Errorf("GetKeywordInfo(%v) class = %v, want %v", tt.token, info.Class, tt.wantInfo.Class)
			}
			if tt.wantInfo.Category != "" && info.Category != tt.wantInfo.Category {
				t.Errorf("GetKeywordInfo(%v) category = %v, want %v", tt.token, info.Category, tt.wantInfo.Category)
			}
			if tt.wantInfo.Aliases != nil {
				if len(info.Aliases) != len(tt.wantInfo.Aliases) {
					t.Errorf("GetKeywordInfo(%v) aliases length = %v, want %v", tt.token, len(info.Aliases), len(tt.wantInfo.Aliases))
				} else {
					for i, alias := range tt.wantInfo.Aliases {
						if info.Aliases[i] != alias {
							t.Errorf("GetKeywordInfo(%v) alias[%d] = %v, want %v", tt.token, i, info.Aliases[i], alias)
						}
					}
				}
			}
		})
	}
}
