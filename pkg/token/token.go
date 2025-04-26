package token

// Token is the type for all COBOL tokens
type Token int

// Position represents a position in the source code
type Position struct {
	Line   int
	Column int
	Offset int
}

// TokenInfo contains information about a token
type TokenInfo struct {
	Type    Token
	Literal string
	Pos     Position
}

const (
	// Special tokens
	ILLEGAL Token = iota
	EOF
	COMMENT
	IDENTIFIER
	STRING
	NUMBER

	// COBOL Divisions
	IDENTIFICATION_DIVISION
	ENVIRONMENT_DIVISION
	DATA_DIVISION
	PROCEDURE_DIVISION

	// COBOL Sections
	CONFIGURATION_SECTION
	INPUT_OUTPUT_SECTION
	FILE_SECTION
	WORKING_STORAGE_SECTION
	LINKAGE_SECTION
	SECTION // Generic SECTION token
	FILE    // Generic FILE token
	LOCAL_STORAGE
	LINKAGE
	PIC
	VALUE
	SOURCE_COMPUTER
	OBJECT_COMPUTER
	WORKING_STORAGE // Generic WORKING-STORAGE token
	TO              // Generic TO token
	COMP            // COMP token for picture clauses

	// Keywords
	PROGRAM_ID
	AUTHOR
	INSTALLATION
	DATE_WRITTEN
	DATE_COMPILED
	SECURITY
	ENVIRONMENT
	CONFIGURATION
	SOURCE
	OBJECT
	COMPUTER
	MEMORY
	SEQUENCE
	SEGMENT_LIMIT
	SPECIAL_NAMES
	DECIMAL_POINT
	CONSOLE

	// Basic keywords
	ACCEPT
	ACCESS
	ADD
	ADVANCING
	AFTER
	ALL
	ALPHABETIC

	// Data description
	LEVEL_NUMBER // 01-49, 66, 77, 88
	REDEFINES
	OCCURS
	TIMES
	DEPENDING
	ON
	INDEXED
	BY_KEYWORD
	ASCENDING
	DESCENDING
	KEY
	IS

	// Figurative constants
	ZERO
	ZEROS
	ZEROES
	SPACE
	SPACES
	HIGH_VALUE
	HIGH_VALUES
	LOW_VALUE
	LOW_VALUES
	QUOTE
	QUOTES
	NULL
	NULLS

	// Special registers
	LENGTH_OF
	ADDRESS_OF
	RETURN_CODE
	WHEN_COMPILED
	TALLY

	// Verbs
	CALL
	COMPUTE
	DISPLAY
	DIVIDE
	MOVE
	MULTIPLY
	PERFORM
	STOP
	SUBTRACT
	INITIALIZE
	SET
	GO
	TO_PROC // TO for procedures
	THRU
	VARYING
	FROM
	BY
	UNTIL

	// Conditions and relations
	IF
	ELSE
	END_IF
	EVALUATE
	WHEN
	END_EVALUATE
	GREATER
	LESS
	EQUAL_TO
	GREATER_THAN
	LESS_THAN
	NOT
	AND
	OR
	THAN
	TRUE
	FALSE

	// Literals
	NUMERIC
	STRING_LIT
	CURRENCY

	// Punctuation
	DOT       // .
	COMMA     // ,
	SEMICOLON // ;
	LPAREN    // (
	RPAREN    // )
	LBRACE    // {
	RBRACE    // }

	// Operators
	PLUS     // +
	MINUS    // -
	ASTERISK // *
	SLASH    // /
	EQUAL    // =

	// Identifiers
	IDENT

	// File control tokens
	ASSIGN
	ORGANIZATION
	SEQUENTIAL
	RANDOM
	DYNAMIC

	// Statement end tokens
	END_PERFORM

	// Exception handling tokens
	TRY
	CATCH
	END_TRY
	FINALLY
	RAISE
	SIZE_ERROR
)

// String constants for tokens
const (
	// String constants for tokens
	ILLEGAL_STR = "ILLEGAL"
	EOF_STR     = "EOF"
	COMMENT_STR = "COMMENT"
	IDENT_STR   = "IDENT"
	INT_STR     = "INT"
	STRING_STR  = "STRING"
	PERIOD_STR  = "."

	// Basic tokens
	SECTION_STR         = "SECTION"
	FILE_STR            = "FILE"
	LOCAL_STORAGE_STR   = "LOCAL-STORAGE"
	LINKAGE_STR         = "LINKAGE"
	PIC_STR             = "PIC"
	VALUE_STR           = "VALUE"
	SOURCE_COMPUTER_STR = "SOURCE-COMPUTER"
	OBJECT_COMPUTER_STR = "OBJECT-COMPUTER"

	// Data description strings
	LEVEL_NUMBER_STR = "LEVEL_NUMBER"
	REDEFINES_STR    = "REDEFINES"
	OCCURS_STR       = "OCCURS"
	TIMES_STR        = "TIMES"
	DEPENDING_STR    = "DEPENDING"
	ON_STR           = "ON"
	INDEXED_STR      = "INDEXED"
	BY_KEYWORD_STR   = "BY"
	ASCENDING_STR    = "ASCENDING"
	DESCENDING_STR   = "DESCENDING"
	KEY_STR          = "KEY"
	IS_STR           = "IS"

	// Figurative constants strings
	ZERO_STR        = "ZERO"
	ZEROS_STR       = "ZEROS"
	ZEROES_STR      = "ZEROES"
	SPACE_STR       = "SPACE"
	SPACES_STR      = "SPACES"
	HIGH_VALUE_STR  = "HIGH-VALUE"
	HIGH_VALUES_STR = "HIGH-VALUES"
	LOW_VALUE_STR   = "LOW-VALUE"
	LOW_VALUES_STR  = "LOW-VALUES"
	QUOTE_STR       = "QUOTE"
	QUOTES_STR      = "QUOTES"
	NULL_STR        = "NULL"
	NULLS_STR       = "NULLS"

	// Special registers strings
	LENGTH_OF_STR     = "LENGTH OF"
	ADDRESS_OF_STR    = "ADDRESS OF"
	RETURN_CODE_STR   = "RETURN-CODE"
	WHEN_COMPILED_STR = "WHEN-COMPILED"
	TALLY_STR         = "TALLY"

	// Verbs strings
	CALL_STR    = "CALL"
	COMPUTE_STR = "COMPUTE"
	UNTIL_STR   = "UNTIL"

	// Conditions strings
	IF_STR           = "IF"
	ELSE_STR         = "ELSE"
	END_IF_STR       = "END-IF"
	EVALUATE_STR     = "EVALUATE"
	WHEN_STR         = "WHEN"
	END_EVALUATE_STR = "END-EVALUATE"
	GREATER_STR      = "GREATER"
	LESS_STR         = "LESS"
	EQUAL_TO_STR     = "EQUAL TO"
	GREATER_THAN_STR = "GREATER THAN"
	LESS_THAN_STR    = "LESS THAN"
	NOT_STR          = "NOT"
	AND_STR          = "AND"
	OR_STR           = "OR"
	THAN_STR         = "THAN"
	TRUE_STR         = "TRUE"
	FALSE_STR        = "FALSE"

	// Literals strings
	NUMERIC_STR    = "NUMERIC"
	STRING_LIT_STR = "STRING"
	CURRENCY_STR   = "$"

	// Punctuation strings
	DOT_STR       = "."
	COMMA_STR     = ","
	SEMICOLON_STR = ";"
	LPAREN_STR    = "("
	RPAREN_STR    = ")"
	LBRACE_STR    = "{"
	RBRACE_STR    = "}"

	// Operators strings
	PLUS_STR     = "+"
	MINUS_STR    = "-"
	ASTERISK_STR = "*"
	SLASH_STR    = "/"
	EQUAL_STR    = "="

	// File control strings
	ASSIGN_STR       = "ASSIGN"
	ORGANIZATION_STR = "ORGANIZATION"
	SEQUENTIAL_STR   = "SEQUENTIAL"
	RANDOM_STR       = "RANDOM"
	DYNAMIC_STR      = "DYNAMIC"

	// Statement end strings
	END_PERFORM_STR = "END-PERFORM"

	// Exception handling strings
	TRY_STR        = "TRY"
	CATCH_STR      = "CATCH"
	END_TRY_STR    = "END-TRY"
	FINALLY_STR    = "FINALLY"
	RAISE_STR      = "RAISE"
	SIZE_ERROR_STR = "SIZE ERROR"

	// Divisions
	IDENTIFICATION_DIV_STR = "IDENTIFICATION DIVISION"
	ENVIRONMENT_DIV_STR    = "ENVIRONMENT DIVISION"
	DATA_DIV_STR           = "DATA DIVISION"
	PROCEDURE_DIV_STR      = "PROCEDURE DIVISION"

	// Sections
	CONFIGURATION_SECT_STR   = "CONFIGURATION SECTION"
	INPUT_OUTPUT_SECT_STR    = "INPUT-OUTPUT SECTION"
	FILE_SECT_STR            = "FILE SECTION"
	WORKING_STORAGE_SECT_STR = "WORKING-STORAGE SECTION"
	LINKAGE_SECT_STR         = "LINKAGE SECTION"

	// Individual keywords
	PROGRAM_ID_STR    = "PROGRAM-ID"
	AUTHOR_STR        = "AUTHOR"
	INSTALLATION_STR  = "INSTALLATION"
	DATE_WRITTEN_STR  = "DATE-WRITTEN"
	DATE_COMPILED_STR = "DATE-COMPILED"
	SECURITY_STR      = "SECURITY"
	ENVIRONMENT_STR   = "ENVIRONMENT"
	CONFIGURATION_STR = "CONFIGURATION"
	SOURCE_STR        = "SOURCE"
	OBJECT_STR        = "OBJECT"
	COMPUTER_STR      = "COMPUTER"
	MEMORY_STR        = "MEMORY"
	SEQUENCE_STR      = "SEQUENCE"
	SEGMENT_LIMIT_STR = "SEGMENT-LIMIT"
	SPECIAL_NAMES_STR = "SPECIAL-NAMES"
	DECIMAL_POINT_STR = "DECIMAL-POINT"
	CONSOLE_STR       = "CONSOLE"
	TO_STR            = "TO"
	COMP_STR          = "COMP-3"
)

var keywords = map[string]Token{
	IDENTIFICATION_DIV_STR:   IDENTIFICATION_DIVISION,
	ENVIRONMENT_DIV_STR:      ENVIRONMENT_DIVISION,
	DATA_DIV_STR:             DATA_DIVISION,
	PROCEDURE_DIV_STR:        PROCEDURE_DIVISION,
	CONFIGURATION_SECT_STR:   CONFIGURATION_SECTION,
	INPUT_OUTPUT_SECT_STR:    INPUT_OUTPUT_SECTION,
	FILE_SECT_STR:            FILE_SECTION,
	WORKING_STORAGE_SECT_STR: WORKING_STORAGE_SECTION,
	LINKAGE_SECT_STR:         LINKAGE_SECTION,
	PROGRAM_ID_STR:           PROGRAM_ID,
	AUTHOR_STR:               AUTHOR,
	INSTALLATION_STR:         INSTALLATION,
	DATE_WRITTEN_STR:         DATE_WRITTEN,
	DATE_COMPILED_STR:        DATE_COMPILED,
	SECURITY_STR:             SECURITY,
	ENVIRONMENT_STR:          ENVIRONMENT,
	CONFIGURATION_STR:        CONFIGURATION,
	SOURCE_STR:               SOURCE,
	OBJECT_STR:               OBJECT,
	COMPUTER_STR:             COMPUTER,
	MEMORY_STR:               MEMORY,
	SEQUENCE_STR:             SEQUENCE,
	SEGMENT_LIMIT_STR:        SEGMENT_LIMIT,
	SPECIAL_NAMES_STR:        SPECIAL_NAMES,
	DECIMAL_POINT_STR:        DECIMAL_POINT,
	CONSOLE_STR:              CONSOLE,
	REDEFINES_STR:            REDEFINES,
	OCCURS_STR:               OCCURS,
	TIMES_STR:                TIMES,
	DEPENDING_STR:            DEPENDING,
	ON_STR:                   ON,
	INDEXED_STR:              INDEXED,
	BY_KEYWORD_STR:           BY_KEYWORD,
	ASCENDING_STR:            ASCENDING,
	DESCENDING_STR:           DESCENDING,
	KEY_STR:                  KEY,
	IS_STR:                   IS,
	ZERO_STR:                 ZERO,
	ZEROS_STR:                ZEROS,
	ZEROES_STR:               ZEROES,
	SPACE_STR:                SPACE,
	SPACES_STR:               SPACES,
	HIGH_VALUE_STR:           HIGH_VALUE,
	HIGH_VALUES_STR:          HIGH_VALUES,
	LOW_VALUE_STR:            LOW_VALUE,
	LOW_VALUES_STR:           LOW_VALUES,
	QUOTE_STR:                QUOTE,
	QUOTES_STR:               QUOTES,
	NULL_STR:                 NULL,
	NULLS_STR:                NULLS,
	LENGTH_OF_STR:            LENGTH_OF,
	ADDRESS_OF_STR:           ADDRESS_OF,
	RETURN_CODE_STR:          RETURN_CODE,
	WHEN_COMPILED_STR:        WHEN_COMPILED,
	TALLY_STR:                TALLY,
	CALL_STR:                 CALL,
	COMPUTE_STR:              COMPUTE,
	UNTIL_STR:                UNTIL,
	IF_STR:                   IF,
	ELSE_STR:                 ELSE,
	END_IF_STR:               END_IF,
	EVALUATE_STR:             EVALUATE,
	WHEN_STR:                 WHEN,
	END_EVALUATE_STR:         END_EVALUATE,
	GREATER_STR:              GREATER,
	LESS_STR:                 LESS,
	EQUAL_TO_STR:             EQUAL_TO,
	GREATER_THAN_STR:         GREATER_THAN,
	LESS_THAN_STR:            LESS_THAN,
	NOT_STR:                  NOT,
	AND_STR:                  AND,
	OR_STR:                   OR,
	THAN_STR:                 THAN,
	TRUE_STR:                 TRUE,
	FALSE_STR:                FALSE,
	ASSIGN_STR:               ASSIGN,
	ORGANIZATION_STR:         ORGANIZATION,
	SEQUENTIAL_STR:           SEQUENTIAL,
	RANDOM_STR:               RANDOM,
	DYNAMIC_STR:              DYNAMIC,
	END_PERFORM_STR:          END_PERFORM,
	TRY_STR:                  TRY,
	CATCH_STR:                CATCH,
	END_TRY_STR:              END_TRY,
	FINALLY_STR:              FINALLY,
	RAISE_STR:                RAISE,
	SIZE_ERROR_STR:           SIZE_ERROR,
	SECTION_STR:              SECTION,
	FILE_STR:                 FILE,
	LOCAL_STORAGE_STR:        LOCAL_STORAGE,
	LINKAGE_STR:              LINKAGE,
	PIC_STR:                  PIC,
	VALUE_STR:                VALUE,
	SOURCE_COMPUTER_STR:      SOURCE_COMPUTER,
	OBJECT_COMPUTER_STR:      OBJECT_COMPUTER,
	TO_STR:                   TO,
	COMP_STR:                 COMP,
}

// String returns the string representation of the token
func (t Token) String() string {
	switch t {
	case ILLEGAL:
		return "ILLEGAL"
	case EOF:
		return "EOF"
	case COMMENT:
		return "COMMENT"
	case IDENTIFICATION_DIVISION:
		return IDENTIFICATION_DIV_STR
	case ENVIRONMENT_DIVISION:
		return ENVIRONMENT_DIV_STR
	case DATA_DIVISION:
		return DATA_DIV_STR
	case PROCEDURE_DIVISION:
		return PROCEDURE_DIV_STR
	case CONFIGURATION_SECTION:
		return CONFIGURATION_SECT_STR
	case INPUT_OUTPUT_SECTION:
		return INPUT_OUTPUT_SECT_STR
	case FILE_SECTION:
		return FILE_SECT_STR
	case WORKING_STORAGE_SECTION:
		return WORKING_STORAGE_SECT_STR
	case LINKAGE_SECTION:
		return LINKAGE_SECT_STR
	case PROGRAM_ID:
		return PROGRAM_ID_STR
	case AUTHOR:
		return AUTHOR_STR
	case INSTALLATION:
		return INSTALLATION_STR
	case DATE_WRITTEN:
		return DATE_WRITTEN_STR
	case DATE_COMPILED:
		return DATE_COMPILED_STR
	case SECURITY:
		return SECURITY_STR
	case ENVIRONMENT:
		return ENVIRONMENT_STR
	case CONFIGURATION:
		return CONFIGURATION_STR
	case SOURCE:
		return SOURCE_STR
	case OBJECT:
		return OBJECT_STR
	case COMPUTER:
		return COMPUTER_STR
	case MEMORY:
		return MEMORY_STR
	case SEQUENCE:
		return SEQUENCE_STR
	case SEGMENT_LIMIT:
		return SEGMENT_LIMIT_STR
	case SPECIAL_NAMES:
		return SPECIAL_NAMES_STR
	case DECIMAL_POINT:
		return DECIMAL_POINT_STR
	case CONSOLE:
		return CONSOLE_STR
	case ACCEPT:
		return "ACCEPT"
	case ACCESS:
		return "ACCESS"
	case ADD:
		return "ADD"
	case ADVANCING:
		return "ADVANCING"
	case AFTER:
		return "AFTER"
	case ALL:
		return "ALL"
	case ALPHABETIC:
		return "ALPHABETIC"
	default:
		return "UNKNOWN"
	}
}

// IsLevelNumber checks if a string represents a valid COBOL level number
func IsLevelNumber(s string) bool {
	// Valid level numbers are 01-49, 66, 77, 88
	switch s {
	case "66", "77", "88":
		return true
	default:
		if len(s) == 2 {
			if s[0] == '0' || s[0] == '1' || s[0] == '2' || s[0] == '3' || s[0] == '4' {
				if s[1] >= '0' && s[1] <= '9' {
					return true
				}
			}
		}
		return false
	}
}

// Lookup determines if an identifier is a keyword
func Lookup(ident string) Token {
	if tok, isKeyword := keywords[ident]; isKeyword {
		return tok
	}
	if IsLevelNumber(ident) {
		return LEVEL_NUMBER
	}
	return IDENT
}
