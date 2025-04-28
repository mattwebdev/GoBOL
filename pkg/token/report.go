package token

// ReportInfo contains information about a COBOL report writer token
type ReportInfo struct {
	Token    Token      // The token representing this report writer
	Class    TokenClass // The token class (always CLASS_REPORT)
	Context  []Token    // Valid in context of these verbs
	Category string     // Logical category
}

// reportInfos maps tokens to their report writer information
var reportInfos = map[Token]ReportInfo{
	REPORT_GROUP: {
		Token:    REPORT_GROUP,
		Class:    CLASS_REPORT,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "report group",
	},
	REPORT_LINE: {
		Token:    REPORT_LINE,
		Class:    CLASS_REPORT,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "report line",
	},
	REPORT_COLUMN: {
		Token:    REPORT_COLUMN,
		Class:    CLASS_REPORT,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "report column",
	},
	REPORT_PAGE: {
		Token:    REPORT_PAGE,
		Class:    CLASS_REPORT,
		Context:  []Token{GO, TO, FROM, WHEN, AFTER, BEFORE, STANDARD, BASED},
		Category: "report page",
	},
	INITIATE: {
		Token:    INITIATE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report",
	},
	TERMINATE: {
		Token:    TERMINATE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report",
	},
	LINE_COUNTER: {
		Token:    LINE_COUNTER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report",
	},
	PAGE_COUNTER: {
		Token:    PAGE_COUNTER,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report",
	},
	NEXT_GROUP: {
		Token:    NEXT_GROUP,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report",
	},
	NEXT_PAGE: {
		Token:    NEXT_PAGE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report",
	},
	DE: {
		Token:    DE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report",
	},
	RH: {
		Token:    RH,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report",
	},
	PH: {
		Token:    PH,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report",
	},
	RF: {
		Token:    RF,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report",
	},
	PF: {
		Token:    PF,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report",
	},
	CH: {
		Token:    CH,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report",
	},
	CF: {
		Token:    CF,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report",
	},
	GROUP_INDICATE: {
		Token:    GROUP_INDICATE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{REPORT},
		Category: "report",
	},
	REPORT_HEADING: {
		Token:    REPORT_HEADING,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "report heading",
	},
	PAGE_HEADING: {
		Token:    PAGE_HEADING,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "page heading",
	},
	CONTROL_HEADING: {
		Token:    CONTROL_HEADING,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "control heading",
	},
	CONTROL_FOOTING: {
		Token:    CONTROL_FOOTING,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "control footing",
	},
	DETAIL_LINE: {
		Token:    DETAIL_LINE,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "detail line",
	},
	PAGE_FOOTING: {
		Token:    PAGE_FOOTING,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "page footing",
	},
	REPORT_FOOTING: {
		Token:    REPORT_FOOTING,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "report footing",
	},
	GROUP_INDICATE_1: {
		Token:    GROUP_INDICATE_1,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "group indicate 1",
	},
	GROUP_INDICATE_2: {
		Token:    GROUP_INDICATE_2,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "group indicate 2",
	},
	GROUP_INDICATE_3: {
		Token:    GROUP_INDICATE_3,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "group indicate 3",
	},
	GROUP_INDICATE_4: {
		Token:    GROUP_INDICATE_4,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "group indicate 4",
	},
	GROUP_INDICATE_5: {
		Token:    GROUP_INDICATE_5,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "group indicate 5",
	},
	GROUP_INDICATE_6: {
		Token:    GROUP_INDICATE_6,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "group indicate 6",
	},
	GROUP_INDICATE_7: {
		Token:    GROUP_INDICATE_7,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "group indicate 7",
	},
	GROUP_INDICATE_8: {
		Token:    GROUP_INDICATE_8,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "group indicate 8",
	},
	GROUP_INDICATE_9: {
		Token:    GROUP_INDICATE_9,
		Class:    CLASS_REPORT,
		Context:  []Token{REPORT},
		Category: "group indicate 9",
	},
}

// IsReport checks if a token represents a report token
func IsReport(t Token) bool {
	_, exists := reportInfos[t]
	return exists
}

// GetReportInfo returns information about a report writer token
func GetReportInfo(tok Token) (ReportInfo, bool) {
	info, ok := reportInfos[tok]
	return info, ok
}
