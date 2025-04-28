package token

// ScreenInfo contains information about a COBOL screen token
type ScreenInfo struct {
	Token    Token      // The token representing this screen element
	Class    TokenClass // The token class
	Context  []Token    // Valid in context of these verbs
	Category string     // Logical category
}

// screenInfos maps tokens to their screen information
var screenInfos = map[Token]ScreenInfo{
	SCREEN_SECTION: {
		Token:    SCREEN_SECTION,
		Class:    CLASS_SECTION,
		Context:  []Token{},
		Category: "screen section",
	},
	BLANK_SCREEN: {
		Token:    BLANK_SCREEN,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SCREEN_SECTION},
		Category: "blank screen",
	},
	BLANK_LINE: {
		Token:    BLANK_LINE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SCREEN_SECTION},
		Category: "blank line",
	},
	BELL: {
		Token:    BELL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SCREEN_SECTION},
		Category: "bell",
	},
	BLINK: {
		Token:    BLINK,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SCREEN_SECTION},
		Category: "blink",
	},
	HIGHLIGHT: {
		Token:    HIGHLIGHT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SCREEN_SECTION},
		Category: "highlight",
	},
	LOWLIGHT: {
		Token:    LOWLIGHT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SCREEN_SECTION},
		Category: "lowlight",
	},
	REVERSE_VIDEO: {
		Token:    REVERSE_VIDEO,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SCREEN_SECTION},
		Category: "reverse video",
	},
	UNDERLINE: {
		Token:    UNDERLINE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SCREEN_SECTION},
		Category: "underline",
	},
	PROMPT: {
		Token:    PROMPT,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SCREEN_SECTION},
		Category: "prompt",
	},
	SECURE: {
		Token:    SECURE,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SCREEN_SECTION},
		Category: "secure",
	},
	AUTO: {
		Token:    AUTO,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SCREEN_SECTION},
		Category: "auto",
	},
	FULL: {
		Token:    FULL,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SCREEN_SECTION},
		Category: "full",
	},
	REQUIRED: {
		Token:    REQUIRED,
		Class:    CLASS_KEYWORD,
		Context:  []Token{SCREEN_SECTION},
		Category: "required",
	},
}

// IsScreen returns true if the token represents a screen token
func IsScreen(tok Token) bool {
	_, ok := screenInfos[tok]
	return ok
}

// GetScreenInfo returns information about a screen token
func GetScreenInfo(tok Token) (ScreenInfo, bool) {
	info, ok := screenInfos[tok]
	return info, ok
}
