package token

import (
	"testing"
)

func TestReportTokenValues(t *testing.T) {
	// Test that report writer tokens start at 900
	if INITIATE < 900 {
		t.Errorf("Expected report writer tokens to start at 900, but INITIATE token is %d", INITIATE)
	}

	// Test that all report writer tokens are unique
	seen := make(map[Token]string)
	tokens := []struct {
		tok  Token
		name string
	}{
		{INITIATE, "INITIATE"},
		{TERMINATE, "TERMINATE"},
		{LINE_COUNTER, "LINE-COUNTER"},
		{PAGE_COUNTER, "PAGE-COUNTER"},
		{NEXT_GROUP, "NEXT GROUP"},
		{NEXT_PAGE, "NEXT PAGE"},
		{DE, "DE"},
		{RH, "RH"},
		{PH, "PH"},
		{RF, "RF"},
		{PF, "PF"},
		{CH, "CH"},
		{CF, "CF"},
		{GROUP_INDICATE, "GROUP INDICATE"},
	}

	for _, tt := range tokens {
		if existing, exists := seen[tt.tok]; exists {
			t.Errorf("Token value collision between %s and %s", tt.name, existing)
		}
		seen[tt.tok] = tt.name
	}

	// Test that token strings match expected values
	for _, tt := range tokens {
		if got := tt.tok.String(); got != tt.name {
			t.Errorf("Token %d String() = %q, want %q", tt.tok, got, tt.name)
		}
	}
}

func TestReportInfo(t *testing.T) {
	tests := []struct {
		name     string
		token    Token
		wantInfo ReportInfo
		wantOk   bool
	}{
		{
			name:  "INITIATE is report token",
			token: INITIATE,
			wantInfo: ReportInfo{
				Token:    INITIATE,
				Class:    CLASS_KEYWORD,
				Context:  []Token{REPORT},
				Category: "report",
			},
			wantOk: true,
		},
		{
			name:     "ADD is not report token",
			token:    ADD,
			wantInfo: ReportInfo{},
			wantOk:   false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotInfo, gotOk := GetReportInfo(tt.token)
			if gotOk != tt.wantOk {
				t.Errorf("IsReport() = %v, want %v", gotOk, tt.wantOk)
			}
			if gotOk {
				if gotInfo.Token != tt.wantInfo.Token {
					t.Errorf("GetReportInfo().Token = %v, want %v", gotInfo.Token, tt.wantInfo.Token)
				}
				if gotInfo.Class != tt.wantInfo.Class {
					t.Errorf("GetReportInfo().Class = %v, want %v", gotInfo.Class, tt.wantInfo.Class)
				}
				if gotInfo.Category != tt.wantInfo.Category {
					t.Errorf("GetReportInfo().Category = %v, want %v", gotInfo.Category, tt.wantInfo.Category)
				}
				if len(gotInfo.Context) != len(tt.wantInfo.Context) {
					t.Errorf("GetReportInfo().Context length = %v, want %v", len(gotInfo.Context), len(tt.wantInfo.Context))
				} else {
					for i, v := range gotInfo.Context {
						if v != tt.wantInfo.Context[i] {
							t.Errorf("GetReportInfo().Context[%d] = %v, want %v", i, v, tt.wantInfo.Context[i])
						}
					}
				}
			}
		})
	}
}

func TestIsReport(t *testing.T) {
	tests := []struct {
		name     string
		token    Token
		expected bool
	}{
		{"INITIATE is report token", INITIATE, true},
		{"TERMINATE is report token", TERMINATE, true},
		{"LINE_COUNTER is report token", LINE_COUNTER, true},
		{"PAGE_COUNTER is report token", PAGE_COUNTER, true},
		{"NEXT_GROUP is report token", NEXT_GROUP, true},
		{"NEXT_PAGE is report token", NEXT_PAGE, true},
		{"DE is report token", DE, true},
		{"RH is report token", RH, true},
		{"PH is report token", PH, true},
		{"RF is report token", RF, true},
		{"PF is report token", PF, true},
		{"CH is report token", CH, true},
		{"CF is report token", CF, true},
		{"GROUP_INDICATE is report token", GROUP_INDICATE, true},
		{"ADD is not report token", ADD, false},
		{"MOVE is not report token", MOVE, false},
		{"ILLEGAL is not report token", ILLEGAL, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsReport(tt.token); got != tt.expected {
				t.Errorf("IsReport(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}
