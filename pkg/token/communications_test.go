package token

import (
	"testing"
)

func TestIsCommunication(t *testing.T) {
	tests := []struct {
		token Token
		want  bool
	}{
		{COMMUNICATION_SECTION, true},
		{CD, true},
		{SYMBOLIC_QUEUE, true},
		{SYMBOLIC_SUB_QUEUE, true},
		{MESSAGE_COUNT, true},
		{MESSAGE_DATE, true},
		{MESSAGE_TIME, true},
		{TEXT_LENGTH, true},
		{END_KEY, true},
		{STATUS_KEY, true},
		{IDENTIFIER, false},
		{PROGRAM_ID, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			if got := IsCommunication(tt.token); got != tt.want {
				t.Errorf("IsCommunication() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestGetCommunicationInfo(t *testing.T) {
	tests := []struct {
		token Token
		want  bool
	}{
		{COMMUNICATION_SECTION, true},
		{CD, true},
		{SYMBOLIC_QUEUE, true},
		{SYMBOLIC_SUB_QUEUE, true},
		{MESSAGE_COUNT, true},
		{MESSAGE_DATE, true},
		{MESSAGE_TIME, true},
		{TEXT_LENGTH, true},
		{END_KEY, true},
		{STATUS_KEY, true},
		{IDENTIFIER, false},
		{PROGRAM_ID, false},
	}

	for _, tt := range tests {
		t.Run(tt.token.String(), func(t *testing.T) {
			info, got := GetCommunicationInfo(tt.token)
			if got != tt.want {
				t.Errorf("GetCommunicationInfo() = %v, want %v", got, tt.want)
			}
			if got && info.Token != tt.token {
				t.Errorf("GetCommunicationInfo().Token = %v, want %v", info.Token, tt.token)
			}
		})
	}
}
