package token

import (
	"reflect"
	"testing"
)

func TestFileOrgTokenValues(t *testing.T) {
	// Test that all file organization tokens are unique
	tests := []struct {
		tok  Token
		name string
	}{
		{LINE_SEQUENTIAL, "LINE SEQUENTIAL"},
		{QUEUE, "QUEUE"},
		{RELATIVE_KEY, "RELATIVE KEY"},
		{RECORD_KEY, "RECORD KEY"},
		{FILE_SHARING, "FILE SHARING"},
		{FILE_LOCKING, "FILE LOCKING"},
		{FILE_RECORDING, "FILE RECORDING"},
		{FILE_ACCESS_MODE, "FILE ACCESS MODE"},
		{FILE_ORGANIZATION_INDEXED, "FILE ORGANIZATION INDEXED"},
		{FILE_ORGANIZATION_RELATIVE, "FILE ORGANIZATION RELATIVE"},
		{FILE_ORGANIZATION_SEQUENTIAL, "FILE ORGANIZATION SEQUENTIAL"},
	}

	// Test that all file organization tokens are unique
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

func TestIsFileOrg(t *testing.T) {
	tests := []struct {
		name     string
		token    Token
		expected bool
	}{
		{"LINE_SEQUENTIAL is file org", LINE_SEQUENTIAL, true},
		{"QUEUE is file org", QUEUE, true},
		{"RELATIVE_KEY is file org", RELATIVE_KEY, true},
		{"RECORD_KEY is file org", RECORD_KEY, true},
		{"FILE_SHARING is file org", FILE_SHARING, true},
		{"FILE_LOCKING is file org", FILE_LOCKING, true},
		{"FILE_RECORDING is file org", FILE_RECORDING, true},
		{"FILE_ACCESS_MODE is file org", FILE_ACCESS_MODE, true},
		{"FILE_ORGANIZATION_INDEXED is file org", FILE_ORGANIZATION_INDEXED, true},
		{"FILE_ORGANIZATION_RELATIVE is file org", FILE_ORGANIZATION_RELATIVE, true},
		{"FILE_ORGANIZATION_SEQUENTIAL is file org", FILE_ORGANIZATION_SEQUENTIAL, true},
		{"MOVE is not file org", MOVE, false},
		{"ADD is not file org", ADD, false},
		{"ILLEGAL is not file org", ILLEGAL, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsFileOrg(tt.token); got != tt.expected {
				t.Errorf("IsFileOrg(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestGetFileOrgInfo(t *testing.T) {
	tests := []struct {
		name     string
		token    Token
		expected FileOrgInfo
		exists   bool
	}{
		{
			name:  "LINE_SEQUENTIAL info",
			token: LINE_SEQUENTIAL,
			expected: FileOrgInfo{
				Token:    LINE_SEQUENTIAL,
				Class:    CLASS_FILE,
				Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
				Category: "file organization",
			},
			exists: true,
		},
		{
			name:  "FILE_SHARING info",
			token: FILE_SHARING,
			expected: FileOrgInfo{
				Token:    FILE_SHARING,
				Class:    CLASS_FILE,
				Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
				Category: "file sharing",
			},
			exists: true,
		},
		{
			name:     "MOVE info",
			token:    MOVE,
			expected: FileOrgInfo{},
			exists:   false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, exists := GetFileOrgInfo(tt.token)
			if exists != tt.exists {
				t.Errorf("GetFileOrgInfo(%v) exists = %v, want %v", tt.token, exists, tt.exists)
			}
			if tt.exists && !reflect.DeepEqual(got, tt.expected) {
				t.Errorf("GetFileOrgInfo(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestFileOrgValidation(t *testing.T) {
	tests := []struct {
		name    string
		token   Token
		context []Token
		wantErr bool
	}{
		{
			name:    "valid file organization in correct context",
			token:   FILE_ORGANIZATION_INDEXED,
			context: []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantErr: false,
		},
		{
			name:    "valid file sharing in correct context",
			token:   FILE_SHARING,
			context: []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantErr: false,
		},
		{
			name:    "valid file locking in correct context",
			token:   FILE_LOCKING,
			context: []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantErr: false,
		},
		{
			name:    "valid file recording in correct context",
			token:   FILE_RECORDING,
			context: []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantErr: false,
		},
		{
			name:    "valid file access mode in correct context",
			token:   FILE_ACCESS_MODE,
			context: []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantErr: false,
		},
		{
			name:    "invalid file organization in wrong context",
			token:   FILE_ORGANIZATION_INDEXED,
			context: []Token{ILLEGAL, EOF},
			wantErr: true,
		},
		{
			name:    "invalid file sharing in wrong context",
			token:   FILE_SHARING,
			context: []Token{ILLEGAL, EOF},
			wantErr: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := ValidateTokenContext(tt.token, tt.context)
			if (err != nil) != tt.wantErr {
				t.Errorf("ValidateTokenContext() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}
