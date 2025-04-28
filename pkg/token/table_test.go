package token

import (
	"testing"
)

func TestIsTable(t *testing.T) {
	tests := []struct {
		token Token
		want  bool
	}{
		{OCCURS, true},
		{DEPENDING_ON, true},
		{ASCENDING, true},
		{DESCENDING, true},
		{INDEXED_BY, true},
		{KEY_IS, true},
		{IDENTIFIER, false},
		{PROGRAM_ID, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := IsTable(tt.token); got != tt.want {
				t.Errorf("IsTable() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestGetTableInfo(t *testing.T) {
	tests := []struct {
		token Token
		want  bool
	}{
		{OCCURS, true},
		{DEPENDING_ON, true},
		{ASCENDING, true},
		{DESCENDING, true},
		{INDEXED_BY, true},
		{KEY_IS, true},
		{IDENTIFIER, false},
		{PROGRAM_ID, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			info, got := GetTableInfo(tt.token)
			if got != tt.want {
				t.Errorf("GetTableInfo() = %v, want %v", got, tt.want)
			}
			if got && info.Token != tt.token {
				t.Errorf("GetTableInfo().Token = %v, want %v", info.Token, tt.token)
			}
		})
	}
}
