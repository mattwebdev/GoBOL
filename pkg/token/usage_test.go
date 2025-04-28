package token

import (
	"testing"
)

func TestIsUsage(t *testing.T) {
	tests := []struct {
		token Token
		want  bool
	}{
		{USAGE_OBJECT, true},
		{USAGE_REFERENCE, true},
		{USAGE_POINTER_64, true},
		{USAGE_FUNCTION_POINTER, true},
		{USAGE_PROGRAM_POINTER, true},
		{USAGE_METHOD_POINTER, true},
		{USAGE_INDEX_64, true},
		{USAGE_INDEX_32, true},
		{IDENTIFIER, false},
		{PROGRAM_ID, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := IsUsage(tt.token); got != tt.want {
				t.Errorf("IsUsage() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestGetUsageInfo(t *testing.T) {
	tests := []struct {
		token Token
		want  bool
	}{
		{USAGE_OBJECT, true},
		{USAGE_REFERENCE, true},
		{USAGE_POINTER_64, true},
		{USAGE_FUNCTION_POINTER, true},
		{USAGE_PROGRAM_POINTER, true},
		{USAGE_METHOD_POINTER, true},
		{USAGE_INDEX_64, true},
		{USAGE_INDEX_32, true},
		{IDENTIFIER, false},
		{PROGRAM_ID, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			info, got := GetUsageInfo(tt.token)
			if got != tt.want {
				t.Errorf("GetUsageInfo() = %v, want %v", got, tt.want)
			}
			if got && info.Token != tt.token {
				t.Errorf("GetUsageInfo().Token = %v, want %v", info.Token, tt.token)
			}
		})
	}
}
