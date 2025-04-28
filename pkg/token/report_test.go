package token

import (
	"reflect"
	"testing"
)

func TestReportTokenValues(t *testing.T) {
	// Test that all report writer tokens are unique
	tests := []struct {
		tok  Token
		name string
	}{
		{REPORT_GROUP, "REPORT GROUP"},
		{REPORT_LINE, "REPORT LINE"},
		{REPORT_COLUMN, "REPORT COLUMN"},
		{REPORT_PAGE, "REPORT PAGE"},
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
		{REPORT_HEADING, "REPORT HEADING"},
		{PAGE_HEADING, "PAGE HEADING"},
		{CONTROL_HEADING, "CONTROL HEADING"},
		{CONTROL_FOOTING, "CONTROL FOOTING"},
		{DETAIL_LINE, "DETAIL LINE"},
		{PAGE_FOOTING, "PAGE FOOTING"},
		{REPORT_FOOTING, "REPORT FOOTING"},
		{GROUP_INDICATE_1, "GROUP INDICATE 1"},
		{GROUP_INDICATE_2, "GROUP INDICATE 2"},
		{GROUP_INDICATE_3, "GROUP INDICATE 3"},
		{GROUP_INDICATE_4, "GROUP INDICATE 4"},
		{GROUP_INDICATE_5, "GROUP INDICATE 5"},
		{GROUP_INDICATE_6, "GROUP INDICATE 6"},
		{GROUP_INDICATE_7, "GROUP INDICATE 7"},
		{GROUP_INDICATE_8, "GROUP INDICATE 8"},
		{GROUP_INDICATE_9, "GROUP INDICATE 9"},
	}

	// Test that all report writer tokens are unique
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
		{"REPORT_GROUP is report", REPORT_GROUP, true},
		{"REPORT_LINE is report", REPORT_LINE, true},
		{"REPORT_COLUMN is report", REPORT_COLUMN, true},
		{"REPORT_PAGE is report", REPORT_PAGE, true},
		{"INITIATE is report", INITIATE, true},
		{"TERMINATE is report", TERMINATE, true},
		{"LINE_COUNTER is report", LINE_COUNTER, true},
		{"PAGE_COUNTER is report", PAGE_COUNTER, true},
		{"NEXT_GROUP is report", NEXT_GROUP, true},
		{"NEXT_PAGE is report", NEXT_PAGE, true},
		{"DE is report", DE, true},
		{"RH is report", RH, true},
		{"PH is report", PH, true},
		{"RF is report", RF, true},
		{"PF is report", PF, true},
		{"CH is report", CH, true},
		{"CF is report", CF, true},
		{"GROUP_INDICATE is report", GROUP_INDICATE, true},
		{"REPORT_HEADING is report", REPORT_HEADING, true},
		{"PAGE_HEADING is report", PAGE_HEADING, true},
		{"CONTROL_HEADING is report", CONTROL_HEADING, true},
		{"CONTROL_FOOTING is report", CONTROL_FOOTING, true},
		{"DETAIL_LINE is report", DETAIL_LINE, true},
		{"PAGE_FOOTING is report", PAGE_FOOTING, true},
		{"REPORT_FOOTING is report", REPORT_FOOTING, true},
		{"GROUP_INDICATE_1 is report", GROUP_INDICATE_1, true},
		{"GROUP_INDICATE_2 is report", GROUP_INDICATE_2, true},
		{"GROUP_INDICATE_3 is report", GROUP_INDICATE_3, true},
		{"GROUP_INDICATE_4 is report", GROUP_INDICATE_4, true},
		{"GROUP_INDICATE_5 is report", GROUP_INDICATE_5, true},
		{"GROUP_INDICATE_6 is report", GROUP_INDICATE_6, true},
		{"GROUP_INDICATE_7 is report", GROUP_INDICATE_7, true},
		{"GROUP_INDICATE_8 is report", GROUP_INDICATE_8, true},
		{"GROUP_INDICATE_9 is report", GROUP_INDICATE_9, true},
		{"MOVE is not report", MOVE, false},
		{"ADD is not report", ADD, false},
		{"ILLEGAL is not report", ILLEGAL, false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := IsReport(tt.token); got != tt.expected {
				t.Errorf("IsReport(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestGetReportInfo(t *testing.T) {
	tests := []struct {
		name     string
		token    Token
		expected ReportInfo
		exists   bool
	}{
		{
			name:  "REPORT_GROUP info",
			token: REPORT_GROUP,
			expected: ReportInfo{
				Token:    REPORT_GROUP,
				Class:    CLASS_REPORT,
				Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
				Category: "report group",
			},
			exists: true,
		},
		{
			name:  "REPORT_LINE info",
			token: REPORT_LINE,
			expected: ReportInfo{
				Token:    REPORT_LINE,
				Class:    CLASS_REPORT,
				Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
				Category: "report line",
			},
			exists: true,
		},
		{
			name:     "MOVE info",
			token:    MOVE,
			expected: ReportInfo{},
			exists:   false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, exists := GetReportInfo(tt.token)
			if exists != tt.exists {
				t.Errorf("GetReportInfo(%v) exists = %v, want %v", tt.token, exists, tt.exists)
			}
			if tt.exists && !reflect.DeepEqual(got, tt.expected) {
				t.Errorf("GetReportInfo(%v) = %v, want %v", tt.token, got, tt.expected)
			}
		})
	}
}

func TestReportValidation(t *testing.T) {
	tests := []struct {
		name    string
		token   Token
		context []Token
		wantErr bool
	}{
		{
			name:    "valid report group in correct context",
			token:   REPORT_GROUP,
			context: []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantErr: false,
		},
		{
			name:    "valid report line in correct context",
			token:   REPORT_LINE,
			context: []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantErr: false,
		},
		{
			name:    "valid report column in correct context",
			token:   REPORT_COLUMN,
			context: []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantErr: false,
		},
		{
			name:    "valid report page in correct context",
			token:   REPORT_PAGE,
			context: []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
			wantErr: false,
		},
		{
			name:    "invalid report group in wrong context",
			token:   REPORT_GROUP,
			context: []Token{ILLEGAL, EOF},
			wantErr: true,
		},
		{
			name:    "invalid report line in wrong context",
			token:   REPORT_LINE,
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
