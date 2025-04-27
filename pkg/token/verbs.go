package token

// COBOL verb tokens start at 700
const (
	// Data manipulation verbs
	MOVE Token = iota + 700
	ADD
	SUBTRACT
	MULTIPLY
	DIVIDE
	COMPUTE
	INITIALIZE

	// I/O verbs
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

	// Control flow verbs
	PERFORM
	GO_TO
	IF
	EVALUATE
	CONTINUE
	EXIT
	STOP
	ALTER
	USE
	RAISE  // Exception handling
	RESUME // Exception handling

	// String manipulation verbs
	STRING_VERB // Renamed to avoid conflict with STRING_LIT
	UNSTRING
	INSPECT

	// Table handling verbs
	SEARCH
	SET
	SORT
	MERGE

	// Memory management verbs
	ALLOCATE
	FREE

	// Data validation verbs
	VALIDATE

	// Report Writer verbs
	GENERATE
	SUPPRESS

	// Program linkage verbs
	CALL
	CANCEL
	GOBACK
	EXIT_PROGRAM
)

// VerbInfo contains information about a COBOL verb's syntax
type VerbInfo struct {
	Token       Token
	Class       TokenClass
	MinParams   int
	MaxParams   int
	Terminators []Token
	Modifiers   []Token
	Patterns    []MultiWordPattern // Possible multi-word patterns for this verb
}

// Define verb patterns
var verbPatterns = map[Token]VerbInfo{
	MOVE: {
		Token:       MOVE,
		Class:       CLASS_VERB,
		MinParams:   2,
		MaxParams:   2,
		Terminators: []Token{OP_PERIOD},
		Modifiers:   []Token{CORRESPONDING, TO},
		Patterns: []MultiWordPattern{
			{
				Parts:    []Token{MOVE, CORRESPONDING, TO},
				Result:   MOVE,
				Optional: []bool{false, false, false},
			},
			{
				Parts:    []Token{MOVE, CORR, TO},
				Result:   MOVE,
				Optional: []bool{false, false, false},
			},
		},
	},
	ADD: {
		Token:       ADD,
		Class:       CLASS_VERB,
		MinParams:   2,
		MaxParams:   -1, // -1 means unlimited
		Terminators: []Token{OP_PERIOD},
		Modifiers:   []Token{TO, GIVING},
		Patterns: []MultiWordPattern{
			{
				Parts:    []Token{ADD, TO},
				Result:   ADD,
				Optional: []bool{false, false},
			},
			{
				Parts:    []Token{ADD, GIVING},
				Result:   ADD,
				Optional: []bool{false, false},
			},
		},
	},
	PERFORM: {
		Token:       PERFORM,
		Class:       CLASS_VERB,
		MinParams:   1,
		MaxParams:   -1,
		Terminators: []Token{OP_PERIOD},
		Modifiers:   []Token{TIMES, UNTIL, VARYING},
		Patterns: []MultiWordPattern{
			{
				Parts:    []Token{PERFORM, TIMES},
				Result:   PERFORM,
				Optional: []bool{false, false},
			},
			{
				Parts:    []Token{PERFORM, UNTIL},
				Result:   PERFORM,
				Optional: []bool{false, false},
			},
			{
				Parts:    []Token{PERFORM, VARYING},
				Result:   PERFORM,
				Optional: []bool{false, false},
			},
		},
	},
	INITIALIZE: {
		Token:       INITIALIZE,
		Class:       CLASS_VERB,
		MinParams:   1,
		MaxParams:   -1,
		Terminators: []Token{OP_PERIOD},
		Modifiers:   []Token{TO, VALUE},
		Patterns: []MultiWordPattern{
			{
				Parts:    []Token{INITIALIZE, TO, VALUE},
				Result:   INITIALIZE,
				Optional: []bool{false, false, false},
			},
		},
	},
	USE: {
		Token:       USE,
		Class:       CLASS_VERB,
		MinParams:   1,
		MaxParams:   -1,
		Terminators: []Token{OP_PERIOD},
		Patterns: []MultiWordPattern{
			{
				Parts:    []Token{USE, AFTER, STANDARD},
				Result:   USE,
				Optional: []bool{false, false, false},
			},
			{
				Parts:    []Token{USE, BEFORE, STANDARD},
				Result:   USE,
				Optional: []bool{false, false, false},
			},
		},
	},
	RELEASE: {
		Token:       RELEASE,
		Class:       CLASS_VERB,
		MinParams:   1,
		MaxParams:   1,
		Terminators: []Token{OP_PERIOD},
		Patterns: []MultiWordPattern{
			{
				Parts:    []Token{RELEASE, FROM},
				Result:   RELEASE,
				Optional: []bool{false, true},
			},
		},
	},
	RETURN: {
		Token:       RETURN,
		Class:       CLASS_VERB,
		MinParams:   1,
		MaxParams:   1,
		Terminators: []Token{OP_PERIOD},
	},
	ALTER: {
		Token:       ALTER,
		Class:       CLASS_VERB,
		MinParams:   2,
		MaxParams:   2,
		Terminators: []Token{OP_PERIOD},
		Patterns: []MultiWordPattern{
			{
				Parts:    []Token{ALTER, TO},
				Result:   ALTER,
				Optional: []bool{false, false},
			},
		},
	},
	GENERATE: {
		Token:       GENERATE,
		Class:       CLASS_VERB,
		MinParams:   1,
		MaxParams:   1,
		Terminators: []Token{OP_PERIOD},
	},
	SUPPRESS: {
		Token:       SUPPRESS,
		Class:       CLASS_VERB,
		MinParams:   1,
		MaxParams:   1,
		Terminators: []Token{OP_PERIOD},
	},
	RAISE: {
		Token:       RAISE,
		Class:       CLASS_VERB,
		MinParams:   1,
		MaxParams:   1,
		Terminators: []Token{OP_PERIOD},
	},
	RESUME: {
		Token:       RESUME,
		Class:       CLASS_VERB,
		MinParams:   0,
		MaxParams:   1,
		Terminators: []Token{OP_PERIOD},
	},
	ALLOCATE: {
		Token:       ALLOCATE,
		Class:       CLASS_VERB,
		MinParams:   1,
		MaxParams:   -1,
		Terminators: []Token{OP_PERIOD},
		Patterns: []MultiWordPattern{
			{
				Parts:    []Token{ALLOCATE, BASED},
				Result:   ALLOCATE,
				Optional: []bool{false, true},
			},
		},
	},
	FREE: {
		Token:       FREE,
		Class:       CLASS_VERB,
		MinParams:   1,
		MaxParams:   -1,
		Terminators: []Token{OP_PERIOD},
	},
	VALIDATE: {
		Token:       VALIDATE,
		Class:       CLASS_VERB,
		MinParams:   1,
		MaxParams:   -1,
		Terminators: []Token{OP_PERIOD},
	},
	// Add more verb patterns as needed...
}

// IsVerb checks if a token is a COBOL verb
func IsVerb(t Token) bool {
	_, ok := verbPatterns[t]
	return ok
}

// GetVerbInfo returns information about a verb's syntax
func GetVerbInfo(t Token) (VerbInfo, bool) {
	info, ok := verbPatterns[t]
	return info, ok
}

// GetVerbPatterns returns all multi-word patterns for a verb
func GetVerbPatterns(t Token) []MultiWordPattern {
	if info, ok := verbPatterns[t]; ok {
		return info.Patterns
	}
	return nil
}
