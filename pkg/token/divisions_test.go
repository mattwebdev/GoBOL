package token

import (
	"testing"
)

func TestDivisionTokenValues(t *testing.T) {
	// Test that division tokens start at 400
	if IDENTIFICATION_DIVISION < 400 {
		t.Errorf("Expected division tokens to start at 400, but IDENTIFICATION_DIVISION token is %d", IDENTIFICATION_DIVISION)
	}

	// Test that all division tokens are unique
	seen := make(map[Token]string)
	tokens := []struct {
		tok  Token
		name string
	}{
		{IDENTIFICATION_DIVISION, "IDENTIFICATION_DIVISION"},
		{ENVIRONMENT_DIVISION, "ENVIRONMENT_DIVISION"},
		{DATA_DIVISION, "DATA_DIVISION"},
		{PROCEDURE_DIVISION, "PROCEDURE_DIVISION"},
		{CONFIGURATION_SECTION, "CONFIGURATION_SECTION"},
		{INPUT_OUTPUT_SECTION, "INPUT_OUTPUT_SECTION"},
		{FILE_SECTION, "FILE_SECTION"},
		{WORKING_STORAGE_SECTION, "WORKING_STORAGE_SECTION"},
		{LOCAL_STORAGE_SECTION, "LOCAL_STORAGE_SECTION"},
		{LINKAGE_SECTION, "LINKAGE_SECTION"},
		{SCREEN_SECTION, "SCREEN_SECTION"},
		{REPORT_SECTION, "REPORT_SECTION"},
		{PROGRAM_ID, "PROGRAM_ID"},
		{AUTHOR, "AUTHOR"},
		{INSTALLATION, "INSTALLATION"},
		{DATE_WRITTEN, "DATE_WRITTEN"},
		{DATE_COMPILED, "DATE_COMPILED"},
		{SECURITY, "SECURITY"},
		{REMARKS, "REMARKS"},
		{SOURCE_COMPUTER, "SOURCE_COMPUTER"},
		{OBJECT_COMPUTER, "OBJECT_COMPUTER"},
		{SPECIAL_NAMES, "SPECIAL_NAMES"},
		{FILE_CONTROL, "FILE_CONTROL"},
		{I_O_CONTROL, "I_O_CONTROL"},
	}

	for _, tt := range tokens {
		if existing, exists := seen[tt.tok]; exists {
			t.Errorf("Token value collision between %s and %s", tt.name, existing)
		}
		seen[tt.tok] = tt.name
	}
}

func TestIsDivision(t *testing.T) {
	tests := []struct {
		token    Token
		expected bool
	}{
		{IDENTIFICATION_DIVISION, true},
		{ENVIRONMENT_DIVISION, true},
		{DATA_DIVISION, true},
		{PROCEDURE_DIVISION, true},
		{CONFIGURATION_SECTION, false}, // section, not division
		{PROGRAM_ID, false},            // paragraph, not division
		{ILLEGAL, false},
		{EOF, false},
		{IDENTIFIER, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := IsDivision(tt.token); got != tt.expected {
				t.Errorf("IsDivision(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestGetDivisionInfo(t *testing.T) {
	tests := []struct {
		token              Token
		expectedExists     bool
		expectedType       DivisionType
		expectedName       string
		expectedCanContain []Token
		expectedRequired   bool
		expectedMinOrder   int
	}{
		{
			token:              IDENTIFICATION_DIVISION,
			expectedExists:     true,
			expectedType:       DIV_IDENTIFICATION,
			expectedName:       "IDENTIFICATION DIVISION",
			expectedCanContain: []Token{PROGRAM_ID, AUTHOR, INSTALLATION, DATE_WRITTEN, DATE_COMPILED, SECURITY, REMARKS},
			expectedRequired:   true,
			expectedMinOrder:   0,
		},
		{
			token:              DATA_DIVISION,
			expectedExists:     true,
			expectedType:       DIV_DATA,
			expectedName:       "DATA DIVISION",
			expectedCanContain: []Token{FILE_SECTION, WORKING_STORAGE_SECTION, LOCAL_STORAGE_SECTION, LINKAGE_SECTION, SCREEN_SECTION, REPORT_SECTION},
			expectedRequired:   false,
			expectedMinOrder:   2,
		},
		{
			token:          ILLEGAL,
			expectedExists: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			info, exists := GetDivisionInfo(tt.token)
			if exists != tt.expectedExists {
				t.Errorf("GetDivisionInfo(%v) exists = %v, want %v", tt.token, exists, tt.expectedExists)
				return
			}
			if !exists {
				return
			}
			if info.Type != tt.expectedType {
				t.Errorf("GetDivisionInfo(%v) type = %v, want %v", tt.token, info.Type, tt.expectedType)
			}
			if info.Name != tt.expectedName {
				t.Errorf("GetDivisionInfo(%v) name = %v, want %v", tt.token, info.Name, tt.expectedName)
			}
			if info.IsRequired != tt.expectedRequired {
				t.Errorf("GetDivisionInfo(%v) isRequired = %v, want %v", tt.token, info.IsRequired, tt.expectedRequired)
			}
			if info.MinOrder != tt.expectedMinOrder {
				t.Errorf("GetDivisionInfo(%v) minOrder = %v, want %v", tt.token, info.MinOrder, tt.expectedMinOrder)
			}
			if tt.expectedCanContain != nil {
				if len(info.CanContain) != len(tt.expectedCanContain) {
					t.Errorf("GetDivisionInfo(%v) canContain length = %v, want %v", tt.token, len(info.CanContain), len(tt.expectedCanContain))
				} else {
					for i, token := range tt.expectedCanContain {
						if info.CanContain[i] != token {
							t.Errorf("GetDivisionInfo(%v) canContain[%d] = %v, want %v", tt.token, i, info.CanContain[i], token)
						}
					}
				}
			}
		})
	}
}

func TestIsSection(t *testing.T) {
	tests := []struct {
		token    Token
		expected bool
	}{
		{CONFIGURATION_SECTION, true},
		{INPUT_OUTPUT_SECTION, true},
		{FILE_SECTION, true},
		{WORKING_STORAGE_SECTION, true},
		{LOCAL_STORAGE_SECTION, true},
		{LINKAGE_SECTION, true},
		{SCREEN_SECTION, true},
		{REPORT_SECTION, true},
		{IDENTIFICATION_DIVISION, false}, // division, not section
		{PROGRAM_ID, false},              // paragraph, not section
		{ILLEGAL, false},
		{EOF, false},
		{IDENTIFIER, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := IsSection(tt.token); got != tt.expected {
				t.Errorf("IsSection(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestGetContainingDivision(t *testing.T) {
	tests := []struct {
		token    Token
		expected Token
	}{
		{PROGRAM_ID, IDENTIFICATION_DIVISION},
		{AUTHOR, IDENTIFICATION_DIVISION},
		{CONFIGURATION_SECTION, ENVIRONMENT_DIVISION},
		{INPUT_OUTPUT_SECTION, ENVIRONMENT_DIVISION},
		{FILE_SECTION, DATA_DIVISION},
		{WORKING_STORAGE_SECTION, DATA_DIVISION},
		{ILLEGAL, ILLEGAL},
		{IDENTIFICATION_DIVISION, ILLEGAL}, // divisions don't have containing divisions
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := GetContainingDivision(tt.token); got != tt.expected {
				t.Errorf("GetContainingDivision(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}
