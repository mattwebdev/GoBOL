package token

import (
	"testing"
)

func TestIsClass(t *testing.T) {
	tests := []struct {
		token Token
		want  bool
	}{
		{CLASS_ALPHABETIC, true},
		{CLASS_ALPHANUMERIC, true},
		{CLASS_NUMERIC, true},
		{CLASS_BOOLEAN, true},
		{CLASS_OBJECT, true},
		{CLASS_REFERENCE, true},
		{CLASS_OBJECT_REFERENCE, true},
		{CLASS_ALPHANUMERIC_EDITED, true},
		{CLASS_NUMERIC_EDITED, true},
		{CLASS_DBCS, true},
		{CLASS_KANJI, true},
		{CLASS_NATIONAL_EDITED, true},
		{CLASS_BOOLEAN_OBJECT, true},
		{CLASS_BOOLEAN_REFERENCE, true},
		{IDENTIFIER, false},
		{PROGRAM_ID, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := IsClass(tt.token); got != tt.want {
				t.Errorf("IsClass() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestGetClassInfo(t *testing.T) {
	tests := []struct {
		token Token
		want  bool
	}{
		{CLASS_ALPHABETIC, true},
		{CLASS_ALPHANUMERIC, true},
		{CLASS_NUMERIC, true},
		{CLASS_BOOLEAN, true},
		{CLASS_OBJECT, true},
		{CLASS_REFERENCE, true},
		{CLASS_OBJECT_REFERENCE, true},
		{CLASS_ALPHANUMERIC_EDITED, true},
		{CLASS_NUMERIC_EDITED, true},
		{CLASS_DBCS, true},
		{CLASS_KANJI, true},
		{CLASS_NATIONAL_EDITED, true},
		{CLASS_BOOLEAN_OBJECT, true},
		{CLASS_BOOLEAN_REFERENCE, true},
		{IDENTIFIER, false},
		{PROGRAM_ID, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			info, got := GetClassInfo(tt.token)
			if got != tt.want {
				t.Errorf("GetClassInfo() = %v, want %v", got, tt.want)
			}
			if got && info.Token != tt.token {
				t.Errorf("GetClassInfo().Token = %v, want %v", info.Token, tt.token)
			}
		})
	}
}
