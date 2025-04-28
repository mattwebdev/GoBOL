package token

// CommunicationInfo contains information about a COBOL communication token
type CommunicationInfo struct {
	Token    Token      // The token representing this communication element
	Class    TokenClass // The token class
	Context  []Token    // Valid in context of these verbs
	Category string     // Logical category
}

// communicationInfos maps tokens to their communication information
var communicationInfos = map[Token]CommunicationInfo{
	COMMUNICATION_SECTION: {
		Token:    COMMUNICATION_SECTION,
		Class:    CLASS_SECTION,
		Context:  []Token{},
		Category: "communication section",
	},
	CD: {
		Token:    CD,
		Class:    CLASS_KEYWORD,
		Context:  []Token{COMMUNICATION_SECTION},
		Category: "communication description",
	},
	SYMBOLIC_QUEUE: {
		Token:    SYMBOLIC_QUEUE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{CD},
		Category: "symbolic queue",
	},
	SYMBOLIC_SUB_QUEUE: {
		Token:    SYMBOLIC_SUB_QUEUE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{CD},
		Category: "symbolic sub-queue",
	},
	MESSAGE_COUNT: {
		Token:    MESSAGE_COUNT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{CD},
		Category: "message count",
	},
	MESSAGE_DATE: {
		Token:    MESSAGE_DATE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{CD},
		Category: "message date",
	},
	MESSAGE_TIME: {
		Token:    MESSAGE_TIME,
		Class:    CLASS_KEYWORD,
		Context:  []Token{CD},
		Category: "message time",
	},
	TEXT_LENGTH: {
		Token:    TEXT_LENGTH,
		Class:    CLASS_KEYWORD,
		Context:  []Token{CD},
		Category: "text length",
	},
	END_KEY: {
		Token:    END_KEY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{CD},
		Category: "end key",
	},
	STATUS_KEY: {
		Token:    STATUS_KEY,
		Class:    CLASS_KEYWORD,
		Context:  []Token{CD},
		Category: "status key",
	},
}

// IsCommunication returns true if the token represents a communication token
func IsCommunication(tok Token) bool {
	_, ok := communicationInfos[tok]
	return ok
}

// GetCommunicationInfo returns information about a communication token
func GetCommunicationInfo(tok Token) (CommunicationInfo, bool) {
	info, ok := communicationInfos[tok]
	return info, ok
}
