package token

// ReportInfo contains information about a COBOL Report Writer token
type ReportInfo struct {
	Token    Token      // The token representing this report writer element
	Class    TokenClass // The token class (always CLASS_KEYWORD for now)
	Context  []Token    // Valid in context of these verbs
	Category string     // Logical category
}

// reportInfos maps tokens to their report writer information
var reportInfos = map[Token]ReportInfo{
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
}

// IsReport returns true if the token represents a report writer token
func IsReport(tok Token) bool {
	_, ok := reportInfos[tok]
	return ok
}

// GetReportInfo returns information about a report writer token
func GetReportInfo(tok Token) (ReportInfo, bool) {
	info, ok := reportInfos[tok]
	return info, ok
}
