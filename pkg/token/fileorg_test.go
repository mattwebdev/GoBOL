package token

import (
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
		name         string
		token        Token
		wantClass    TokenClass
		wantCategory string
		wantContext  []Token
		wantOk       bool
	}{
		{
			name:         "LINE_SEQUENTIAL info",
			token:        LINE_SEQUENTIAL,
			wantClass:    CLASS_FILE,
			wantCategory: "file organization",
			wantContext:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantOk:       true,
		},
		{
			name:         "QUEUE info",
			token:        QUEUE,
			wantClass:    CLASS_FILE,
			wantCategory: "file organization",
			wantContext:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantOk:       true,
		},
		{
			name:         "RELATIVE_KEY info",
			token:        RELATIVE_KEY,
			wantClass:    CLASS_FILE,
			wantCategory: "file organization",
			wantContext:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantOk:       true,
		},
		{
			name:         "RECORD_KEY info",
			token:        RECORD_KEY,
			wantClass:    CLASS_FILE,
			wantCategory: "file organization",
			wantContext:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantOk:       true,
		},
		{
			name:   "MOVE has no file org info",
			token:  MOVE,
			wantOk: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotInfo, gotOk := GetFileOrgInfo(tt.token)
			if gotOk != tt.wantOk {
				t.Errorf("GetFileOrgInfo(%v) ok = %v, want %v", tt.token, gotOk, tt.wantOk)
			}
			if gotOk {
				if gotInfo.Class != tt.wantClass {
					t.Errorf("GetFileOrgInfo(%v).Class = %v, want %v", tt.token, gotInfo.Class, tt.wantClass)
				}
				if gotInfo.Category != tt.wantCategory {
					t.Errorf("GetFileOrgInfo(%v).Category = %v, want %v", tt.token, gotInfo.Category, tt.wantCategory)
				}
				if len(gotInfo.Context) != len(tt.wantContext) {
					t.Errorf("GetFileOrgInfo(%v).Context length = %v, want %v", tt.token, len(gotInfo.Context), len(tt.wantContext))
				} else {
					for i, v := range gotInfo.Context {
						if v != tt.wantContext[i] {
							t.Errorf("GetFileOrgInfo(%v).Context[%d] = %v, want %v", tt.token, i, v, tt.wantContext[i])
						}
					}
				}
			}
		})
	}
}
