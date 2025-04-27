package token

// Division tokens start at 400
const (
	// Divisions
	IDENTIFICATION_DIVISION Token = iota + 400
	ENVIRONMENT_DIVISION
	DATA_DIVISION
	PROCEDURE_DIVISION

	// Common sections
	CONFIGURATION_SECTION
	INPUT_OUTPUT_SECTION
	FILE_SECTION
	WORKING_STORAGE_SECTION
	LOCAL_STORAGE_SECTION
	LINKAGE_SECTION
	SCREEN_SECTION
	REPORT_SECTION

	// Identification Division paragraphs
	PROGRAM_ID
	AUTHOR
	INSTALLATION
	DATE_WRITTEN
	DATE_COMPILED
	SECURITY
	REMARKS

	// Environment Division paragraphs
	SOURCE_COMPUTER
	OBJECT_COMPUTER
	SPECIAL_NAMES
	FILE_CONTROL
	I_O_CONTROL
)

// DivisionType represents the type of a division or section
type DivisionType int

const (
	DIV_UNKNOWN DivisionType = iota
	DIV_IDENTIFICATION
	DIV_ENVIRONMENT
	DIV_DATA
	DIV_PROCEDURE
)

// DivisionInfo contains information about a COBOL division
type DivisionInfo struct {
	Token      Token
	Type       DivisionType
	Name       string
	AllowedIn  []Token // What can contain this division
	CanContain []Token // What this division can contain
	IsRequired bool    // Whether this division is required
	MinOrder   int     // Minimum order in the program (0-based)
}

// SectionInfo contains information about a COBOL section
type SectionInfo struct {
	Token      Token
	Division   Token // Which division this section belongs to
	Name       string
	IsRequired bool
	MinOrder   int // Minimum order within its division (0-based)
}

// Division definitions
var divisionInfo = map[Token]DivisionInfo{
	IDENTIFICATION_DIVISION: {
		Token:      IDENTIFICATION_DIVISION,
		Type:       DIV_IDENTIFICATION,
		Name:       "IDENTIFICATION DIVISION",
		CanContain: []Token{PROGRAM_ID, AUTHOR, INSTALLATION, DATE_WRITTEN, DATE_COMPILED, SECURITY, REMARKS},
		IsRequired: true,
		MinOrder:   0,
	},
	ENVIRONMENT_DIVISION: {
		Token:      ENVIRONMENT_DIVISION,
		Type:       DIV_ENVIRONMENT,
		Name:       "ENVIRONMENT DIVISION",
		CanContain: []Token{CONFIGURATION_SECTION, INPUT_OUTPUT_SECTION},
		IsRequired: false,
		MinOrder:   1,
	},
	DATA_DIVISION: {
		Token:      DATA_DIVISION,
		Type:       DIV_DATA,
		Name:       "DATA DIVISION",
		CanContain: []Token{FILE_SECTION, WORKING_STORAGE_SECTION, LOCAL_STORAGE_SECTION, LINKAGE_SECTION, SCREEN_SECTION, REPORT_SECTION},
		IsRequired: false,
		MinOrder:   2,
	},
	PROCEDURE_DIVISION: {
		Token:      PROCEDURE_DIVISION,
		Type:       DIV_PROCEDURE,
		Name:       "PROCEDURE DIVISION",
		IsRequired: true,
		MinOrder:   3,
	},
}

// IsDivision checks if a token is a division
func IsDivision(t Token) bool {
	_, ok := divisionInfo[t]
	return ok
}

// GetDivisionInfo returns information about a division
func GetDivisionInfo(t Token) (DivisionInfo, bool) {
	info, ok := divisionInfo[t]
	return info, ok
}

// IsSection checks if a token is a section
func IsSection(t Token) bool {
	return t >= CONFIGURATION_SECTION && t <= REPORT_SECTION
}

// GetContainingDivision returns the division that can contain a given token
func GetContainingDivision(t Token) Token {
	for div, info := range divisionInfo {
		for _, allowed := range info.CanContain {
			if allowed == t {
				return div
			}
		}
	}
	return ILLEGAL
}
