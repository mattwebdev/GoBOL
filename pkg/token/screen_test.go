package token

import (
	"testing"
)

func TestIsScreen(t *testing.T) {
	tests := []struct {
		token Token
		want  bool
	}{
		{SCREEN_SECTION, true},
		{BLANK_SCREEN, true},
		{BLANK_LINE, true},
		{BELL, true},
		{BLINK, true},
		{HIGHLIGHT, true},
		{LOWLIGHT, true},
		{REVERSE_VIDEO, true},
		{UNDERLINE, true},
		{PROMPT, true},
		{SECURE, true},
		{AUTO, true},
		{FULL, true},
		{REQUIRED, true},
		{IDENTIFIER, false},
		{PROGRAM_ID, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := IsScreen(tt.token); got != tt.want {
				t.Errorf("IsScreen() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestGetScreenInfo(t *testing.T) {
	tests := []struct {
		token Token
		want  bool
	}{
		{SCREEN_SECTION, true},
		{BLANK_SCREEN, true},
		{BLANK_LINE, true},
		{BELL, true},
		{BLINK, true},
		{HIGHLIGHT, true},
		{LOWLIGHT, true},
		{REVERSE_VIDEO, true},
		{UNDERLINE, true},
		{PROMPT, true},
		{SECURE, true},
		{AUTO, true},
		{FULL, true},
		{REQUIRED, true},
		{IDENTIFIER, false},
		{PROGRAM_ID, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			info, got := GetScreenInfo(tt.token)
			if got != tt.want {
				t.Errorf("GetScreenInfo() = %v, want %v", got, tt.want)
			}
			if got && info.Token != tt.token {
				t.Errorf("GetScreenInfo().Token = %v, want %v", info.Token, tt.token)
			}
		})
	}
}
