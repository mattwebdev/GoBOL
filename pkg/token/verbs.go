package token

// Verb tokens start at 100 to avoid collision with special tokens
const (
	// Basic verbs
	MOVE Token = iota + 100
	ADD
	SUBTRACT
	MULTIPLY
	DIVIDE
	COMPUTE
	INITIALIZE
	ACCEPT
	DISPLAY
	READ
	WRITE
	REWRITE
	DELETE
	START
	OPEN
	CLOSE
	RELEASE
	RETURN
	PERFORM
	GO_TO
	IF
	EVALUATE
	CONTINUE
	EXIT
	STOP
	ALTER
	USE
	STRING_VERB
	UNSTRING
	INSPECT
	SEARCH
	SET
	SORT
	MERGE
	GENERATE
	SUPPRESS
	CALL
	CANCEL
	GOBACK
	EXIT_PROGRAM
	// Additional verbs
	RAISE
	RESUME
	ALLOCATE
	FREE
	VALIDATE
)

// VerbInfo contains information about a COBOL verb
type VerbInfo struct {
	Token       Token
	Category    string  // The verb category (arithmetic, i/o, control flow, etc)
	EndToken    Token   // The corresponding END-* token if applicable
	Terminators []Token // Valid statement terminators
}

// Define verb information
var verbInfos = map[Token]VerbInfo{
	MOVE: {
		Token:    MOVE,
		Category: "data movement",
	},
	ADD: {
		Token:    ADD,
		Category: "arithmetic",
		EndToken: END_ADD,
	},
	SUBTRACT: {
		Token:    SUBTRACT,
		Category: "arithmetic",
		EndToken: END_SUBTRACT,
	},
	MULTIPLY: {
		Token:    MULTIPLY,
		Category: "arithmetic",
		EndToken: END_MULTIPLY,
	},
	DIVIDE: {
		Token:    DIVIDE,
		Category: "arithmetic",
		EndToken: END_DIVIDE,
	},
	COMPUTE: {
		Token:    COMPUTE,
		Category: "arithmetic",
		EndToken: END_COMPUTE,
	},
	INITIALIZE: {
		Token:    INITIALIZE,
		Category: "data movement",
		EndToken: END_INITIALIZE,
	},
	ACCEPT: {
		Token:    ACCEPT,
		Category: "i/o",
		EndToken: END_ACCEPT,
	},
	DISPLAY: {
		Token:    DISPLAY,
		Category: "i/o",
		EndToken: END_DISPLAY,
	},
	READ: {
		Token:    READ,
		Category: "i/o",
		EndToken: END_READ,
	},
	WRITE: {
		Token:    WRITE,
		Category: "i/o",
		EndToken: END_WRITE,
	},
	REWRITE: {
		Token:    REWRITE,
		Category: "i/o",
		EndToken: END_REWRITE,
	},
	DELETE: {
		Token:    DELETE,
		Category: "i/o",
		EndToken: END_DELETE,
	},
	START: {
		Token:    START,
		Category: "i/o",
		EndToken: END_START,
	},
	OPEN: {
		Token:    OPEN,
		Category: "i/o",
	},
	CLOSE: {
		Token:    CLOSE,
		Category: "i/o",
	},
	RELEASE: {
		Token:    RELEASE,
		Category: "i/o",
	},
	RETURN: {
		Token:    RETURN,
		Category: "i/o",
		EndToken: END_RETURN,
	},
	PERFORM: {
		Token:    PERFORM,
		Category: "control flow",
		EndToken: END_PERFORM,
	},
	GO_TO: {
		Token:    GO_TO,
		Category: "control flow",
	},
	IF: {
		Token:    IF,
		Category: "control flow",
		EndToken: END_IF,
	},
	EVALUATE: {
		Token:    EVALUATE,
		Category: "control flow",
		EndToken: END_EVALUATE,
	},
	CONTINUE: {
		Token:    CONTINUE,
		Category: "control flow",
	},
	EXIT: {
		Token:    EXIT,
		Category: "control flow",
	},
	STOP: {
		Token:    STOP,
		Category: "control flow",
	},
	ALTER: {
		Token:    ALTER,
		Category: "control flow",
	},
	USE: {
		Token:    USE,
		Category: "procedure",
	},
	STRING_VERB: {
		Token:    STRING_VERB,
		Category: "string handling",
		EndToken: END_STRING,
	},
	UNSTRING: {
		Token:    UNSTRING,
		Category: "string handling",
		EndToken: END_UNSTRING,
	},
	INSPECT: {
		Token:    INSPECT,
		Category: "string handling",
	},
	SEARCH: {
		Token:    SEARCH,
		Category: "table handling",
		EndToken: END_SEARCH,
	},
	SET: {
		Token:    SET,
		Category: "data movement",
	},
	SORT: {
		Token:    SORT,
		Category: "table handling",
	},
	MERGE: {
		Token:    MERGE,
		Category: "table handling",
	},
	GENERATE: {
		Token:    GENERATE,
		Category: "report writer",
	},
	SUPPRESS: {
		Token:    SUPPRESS,
		Category: "report writer",
	},
	CALL: {
		Token:    CALL,
		Category: "procedure",
		EndToken: END_CALL,
	},
	CANCEL: {
		Token:    CANCEL,
		Category: "procedure",
	},
	GOBACK: {
		Token:    GOBACK,
		Category: "procedure",
	},
	EXIT_PROGRAM: {
		Token:    EXIT_PROGRAM,
		Category: "procedure",
	},
	RAISE: {
		Token:    RAISE,
		Category: "exception handling",
	},
	RESUME: {
		Token:    RESUME,
		Category: "exception handling",
	},
	ALLOCATE: {
		Token:    ALLOCATE,
		Category: "memory management",
	},
	FREE: {
		Token:    FREE,
		Category: "memory management",
	},
	VALIDATE: {
		Token:    VALIDATE,
		Category: "data validation",
	},
}

// IsVerb checks if a token is a COBOL verb
func IsVerb(t Token) bool {
	_, ok := verbInfos[t]
	return ok
}

// GetVerbInfo returns information about a verb token
func GetVerbInfo(t Token) (VerbInfo, bool) {
	info, ok := verbInfos[t]
	return info, ok
}

// GetVerbCategory returns the category of a verb token
func GetVerbCategory(t Token) string {
	if info, ok := verbInfos[t]; ok {
		return info.Category
	}
	return ""
}
