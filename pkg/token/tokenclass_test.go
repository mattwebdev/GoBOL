package token

import (
	"testing"
)

func TestTokenClassValues(t *testing.T) {
	// Test that token classes are sequential and start from 0
	tests := []struct {
		class TokenClass
		want  int
	}{
		{CLASS_UNKNOWN, 0},
		{CLASS_VERB, 1},
		{CLASS_KEYWORD, 2},
		{CLASS_MODIFIER, 3},
		{CLASS_IDENTIFIER, 4},
		{CLASS_LITERAL, 5},
		{CLASS_OPERATOR, 6},
		{CLASS_SEPARATOR, 7},
		{CLASS_DIVISION, 8},
		{CLASS_SECTION, 9},
		{CLASS_DATATYPE, 10},
		{CLASS_SCOPE, 11},
		{CLASS_CONDITION, 12},
		{CLASS_FILE, 13},
	}

	for _, tt := range tests {
		if int(tt.class) != tt.want {
			t.Errorf("TokenClass %v has value %d, want %d", tt.class, tt.class, tt.want)
		}
	}
}

func TestIsValidClass(t *testing.T) {
	tests := []struct {
		name      string
		class     TokenClass
		wantValid bool
	}{
		{"CLASS_UNKNOWN is valid", CLASS_UNKNOWN, true},
		{"CLASS_VERB is valid", CLASS_VERB, true},
		{"CLASS_KEYWORD is valid", CLASS_KEYWORD, true},
		{"CLASS_MODIFIER is valid", CLASS_MODIFIER, true},
		{"CLASS_IDENTIFIER is valid", CLASS_IDENTIFIER, true},
		{"CLASS_LITERAL is valid", CLASS_LITERAL, true},
		{"CLASS_OPERATOR is valid", CLASS_OPERATOR, true},
		{"CLASS_SEPARATOR is valid", CLASS_SEPARATOR, true},
		{"CLASS_DIVISION is valid", CLASS_DIVISION, true},
		{"CLASS_SECTION is valid", CLASS_SECTION, true},
		{"CLASS_DATATYPE is valid", CLASS_DATATYPE, true},
		{"CLASS_SCOPE is valid", CLASS_SCOPE, true},
		{"CLASS_CONDITION is valid", CLASS_CONDITION, true},
		{"CLASS_FILE is valid", CLASS_FILE, true},
		{"Negative value is invalid", TokenClass(-1), false},
		{"Value too large is invalid", TokenClass(14), false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsValidClass(tt.class); got != tt.wantValid {
				t.Errorf("IsValidClass(%v) = %v, want %v", tt.class, got, tt.wantValid)
			}
		})
	}
}

func TestTokenClass_String(t *testing.T) {
	tests := []struct {
		name  string
		class TokenClass
		want  string
	}{
		{"CLASS_UNKNOWN string", CLASS_UNKNOWN, "UNKNOWN"},
		{"CLASS_VERB string", CLASS_VERB, "VERB"},
		{"CLASS_KEYWORD string", CLASS_KEYWORD, "KEYWORD"},
		{"CLASS_MODIFIER string", CLASS_MODIFIER, "MODIFIER"},
		{"CLASS_IDENTIFIER string", CLASS_IDENTIFIER, "IDENTIFIER"},
		{"CLASS_LITERAL string", CLASS_LITERAL, "LITERAL"},
		{"CLASS_OPERATOR string", CLASS_OPERATOR, "OPERATOR"},
		{"CLASS_SEPARATOR string", CLASS_SEPARATOR, "SEPARATOR"},
		{"CLASS_DIVISION string", CLASS_DIVISION, "DIVISION"},
		{"CLASS_SECTION string", CLASS_SECTION, "SECTION"},
		{"CLASS_DATATYPE string", CLASS_DATATYPE, "DATATYPE"},
		{"CLASS_SCOPE string", CLASS_SCOPE, "SCOPE"},
		{"CLASS_CONDITION string", CLASS_CONDITION, "CONDITION"},
		{"CLASS_FILE string", CLASS_FILE, "FILE"},
		{"Invalid class string", TokenClass(99), "INVALID"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.class.String(); got != tt.want {
				t.Errorf("TokenClass(%v).String() = %v, want %v", tt.class, got, tt.want)
			}
		})
	}
}
