package token

import (
	"testing"
)

func TestDatatypeTokenValues(t *testing.T) {
	// Test that all datatype tokens are unique
	tests := []struct {
		tok  Token
		name string
	}{
		{BINARY_CHAR, "BINARY-CHAR"},
		{BINARY_SHORT, "BINARY-SHORT"},
		{BINARY_LONG, "BINARY-LONG"},
		{BINARY_DOUBLE, "BINARY-DOUBLE"},
		{FLOAT_SHORT, "FLOAT-SHORT"},
		{FLOAT_LONG, "FLOAT-LONG"},
		{FLOAT_EXTENDED, "FLOAT-EXTENDED"},
		{POINTER_32, "POINTER-32"},
		{PROCEDURE_POINTER, "PROCEDURE-POINTER"},
		{OBJECT, "OBJECT"},
		{REFERENCE, "REFERENCE"},
		{NATIONAL_TYPE, "NATIONAL"},
		{PACKED_DECIMAL_TYPE, "PACKED-DECIMAL"},
		{DISPLAY_1_TYPE, "DISPLAY-1"},
	}

	// Test that all datatype tokens are unique
	seen := make(map[Token]string)
	for _, tt := range tests {
		if existing, exists := seen[tt.tok]; exists {
			t.Errorf("Token value collision between %s and %s", tt.name, existing)
		}
		seen[tt.tok] = tt.name
	}

	// Test that token strings match expected values
	for _, tt := range tests {
		if got := tt.tok.String(); got != tt.name {
			t.Errorf("Token %d String() = %q, want %q", tt.tok, got, tt.name)
		}
	}
}

func TestIsDataType(t *testing.T) {
	tests := []struct {
		name     string
		token    Token
		expected bool
	}{
		{"BINARY_CHAR is datatype", BINARY_CHAR, true},
		{"BINARY_SHORT is datatype", BINARY_SHORT, true},
		{"BINARY_LONG is datatype", BINARY_LONG, true},
		{"BINARY_DOUBLE is datatype", BINARY_DOUBLE, true},
		{"FLOAT_SHORT is datatype", FLOAT_SHORT, true},
		{"FLOAT_LONG is datatype", FLOAT_LONG, true},
		{"FLOAT_EXTENDED is datatype", FLOAT_EXTENDED, true},
		{"POINTER_32 is datatype", POINTER_32, true},
		{"PROCEDURE_POINTER is datatype", PROCEDURE_POINTER, true},
		{"OBJECT is datatype", OBJECT, true},
		{"REFERENCE is datatype", REFERENCE, true},
		{"NATIONAL_TYPE is datatype", NATIONAL_TYPE, true},
		{"PACKED_DECIMAL_TYPE is datatype", PACKED_DECIMAL_TYPE, true},
		{"DISPLAY_1_TYPE is datatype", DISPLAY_1_TYPE, true},
		{"MOVE is not datatype", MOVE, false},
		{"ADD is not datatype", ADD, false},
		{"ILLEGAL is not datatype", ILLEGAL, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsDataType(tt.token); got != tt.expected {
				t.Errorf("IsDataType(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestGetDataTypeInfo(t *testing.T) {
	tests := []struct {
		name         string
		token        Token
		wantClass    string
		wantCategory string
		wantUsage    string
		wantOk       bool
	}{
		{
			name:         "BINARY_CHAR info",
			token:        BINARY_CHAR,
			wantClass:    "numeric",
			wantCategory: "binary",
			wantUsage:    "BINARY-CHAR",
			wantOk:       true,
		},
		{
			name:         "BINARY_SHORT info",
			token:        BINARY_SHORT,
			wantClass:    "numeric",
			wantCategory: "binary",
			wantUsage:    "BINARY-SHORT",
			wantOk:       true,
		},
		{
			name:         "FLOAT_SHORT info",
			token:        FLOAT_SHORT,
			wantClass:    "numeric",
			wantCategory: "float",
			wantUsage:    "FLOAT-SHORT",
			wantOk:       true,
		},
		{
			name:         "POINTER_32 info",
			token:        POINTER_32,
			wantClass:    "pointer",
			wantCategory: "pointer",
			wantUsage:    "POINTER-32",
			wantOk:       true,
		},
		{
			name:         "PROCEDURE_POINTER info",
			token:        PROCEDURE_POINTER,
			wantClass:    "pointer",
			wantCategory: "procedure-pointer",
			wantUsage:    "PROCEDURE-POINTER",
			wantOk:       true,
		},
		{
			name:         "REFERENCE info",
			token:        REFERENCE,
			wantClass:    "object",
			wantCategory: "reference",
			wantUsage:    "REFERENCE",
			wantOk:       true,
		},
		{
			name:         "NATIONAL_TYPE info",
			token:        NATIONAL_TYPE,
			wantClass:    "national",
			wantCategory: "character",
			wantUsage:    "NATIONAL",
			wantOk:       true,
		},
		{
			name:         "PACKED_DECIMAL_TYPE info",
			token:        PACKED_DECIMAL_TYPE,
			wantClass:    "numeric",
			wantCategory: "packed-decimal",
			wantUsage:    "PACKED-DECIMAL",
			wantOk:       true,
		},
		{
			name:         "DISPLAY_1_TYPE info",
			token:        DISPLAY_1_TYPE,
			wantClass:    "alphanumeric",
			wantCategory: "display",
			wantUsage:    "DISPLAY-1",
			wantOk:       true,
		},
		{
			name:   "MOVE has no datatype info",
			token:  MOVE,
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotInfo, gotOk := GetDataTypeInfo(tt.token)
			if gotOk != tt.wantOk {
				t.Errorf("GetDataTypeInfo(%v) ok = %v, want %v", tt.token, gotOk, tt.wantOk)
			}
			if gotOk {
				if gotInfo.Class != tt.wantClass {
					t.Errorf("GetDataTypeInfo(%v).Class = %v, want %v", tt.token, gotInfo.Class, tt.wantClass)
				}
				if gotInfo.Category != tt.wantCategory {
					t.Errorf("GetDataTypeInfo(%v).Category = %v, want %v", tt.token, gotInfo.Category, tt.wantCategory)
				}
				if gotInfo.Usage != tt.wantUsage {
					t.Errorf("GetDataTypeInfo(%v).Usage = %v, want %v", tt.token, gotInfo.Usage, tt.wantUsage)
				}
			}
		})
	}
}

func TestGetDataTypeCategory(t *testing.T) {
	tests := []struct {
		name         string
		token        Token
		wantCategory string
	}{
		{"BINARY_CHAR category", BINARY_CHAR, "binary"},
		{"FLOAT_SHORT category", FLOAT_SHORT, "float"},
		{"POINTER_32 category", POINTER_32, "pointer"},
		{"PROCEDURE_POINTER category", PROCEDURE_POINTER, "procedure-pointer"},
		{"OBJECT category", OBJECT, "object"},
		{"REFERENCE category", REFERENCE, "reference"},
		{"NATIONAL_TYPE category", NATIONAL_TYPE, "character"},
		{"PACKED_DECIMAL_TYPE category", PACKED_DECIMAL_TYPE, "packed-decimal"},
		{"DISPLAY_1_TYPE category", DISPLAY_1_TYPE, "display"},
		{"MOVE has no category", MOVE, ""},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := GetDataTypeCategory(tt.token); got != tt.wantCategory {
				t.Errorf("GetDataTypeCategory(%v) = %v, want %v", tt.token, got, tt.wantCategory)
			}
		})
	}
}

func TestGetDataTypeUsage(t *testing.T) {
	tests := []struct {
		name      string
		token     Token
		wantUsage string
	}{
		{"BINARY_CHAR usage", BINARY_CHAR, "BINARY-CHAR"},
		{"FLOAT_SHORT usage", FLOAT_SHORT, "FLOAT-SHORT"},
		{"POINTER_32 usage", POINTER_32, "POINTER-32"},
		{"PROCEDURE_POINTER usage", PROCEDURE_POINTER, "PROCEDURE-POINTER"},
		{"OBJECT usage", OBJECT, "OBJECT"},
		{"REFERENCE usage", REFERENCE, "REFERENCE"},
		{"NATIONAL_TYPE usage", NATIONAL_TYPE, "NATIONAL"},
		{"PACKED_DECIMAL_TYPE usage", PACKED_DECIMAL_TYPE, "PACKED-DECIMAL"},
		{"DISPLAY_1_TYPE usage", DISPLAY_1_TYPE, "DISPLAY-1"},
		{"MOVE has no usage", MOVE, ""},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := GetDataTypeUsage(tt.token); got != tt.wantUsage {
				t.Errorf("GetDataTypeUsage(%v) = %v, want %v", tt.token, got, tt.wantUsage)
			}
		})
	}
}
