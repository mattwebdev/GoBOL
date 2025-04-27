package token

// TokenClass represents the classification of a token
type TokenClass int

const (
	CLASS_UNKNOWN    TokenClass = iota
	CLASS_VERB                  // Primary COBOL verbs (MOVE, ADD, etc)
	CLASS_KEYWORD               // Secondary keywords (TO, FROM, etc)
	CLASS_MODIFIER              // Modifiers (CORRESPONDING, etc)
	CLASS_IDENTIFIER            // User-defined names
	CLASS_LITERAL               // Constants and literals
	CLASS_OPERATOR              // Mathematical and logical operators
	CLASS_SEPARATOR             // Dots, commas, etc
	CLASS_DIVISION              // Division headers
	CLASS_SECTION               // Section headers
)

// TokenGroup represents a logical grouping of tokens that form a complete COBOL command
type TokenGroup struct {
	MainToken   TokenInfo   // The primary verb or command
	Modifiers   []TokenInfo // Modifying keywords (CORRESPONDING, etc)
	Parameters  []TokenInfo // The parameters/operands
	Terminators []TokenInfo // Statement terminators (period, etc)
	Context     struct {    // Contextual information
		Division string // Current division
		Section  string // Current section
		Area     rune   // A or B
	}
}

// NewTokenGroup creates a new token group with the main token
func NewTokenGroup(main TokenInfo) *TokenGroup {
	return &TokenGroup{
		MainToken:   main,
		Modifiers:   make([]TokenInfo, 0),
		Parameters:  make([]TokenInfo, 0),
		Terminators: make([]TokenInfo, 0),
	}
}

// AddModifier adds a modifier token to the group
func (g *TokenGroup) AddModifier(t TokenInfo) {
	g.Modifiers = append(g.Modifiers, t)
}

// AddParameter adds a parameter token to the group
func (g *TokenGroup) AddParameter(t TokenInfo) {
	g.Parameters = append(g.Parameters, t)
}

// AddTerminator adds a terminator token to the group
func (g *TokenGroup) AddTerminator(t TokenInfo) {
	g.Terminators = append(g.Terminators, t)
}

// IsComplete checks if the token group forms a complete valid statement
func (g *TokenGroup) IsComplete() bool {
	// This will need to be implemented based on COBOL grammar rules
	// For now, we'll just check if we have a main token and at least one terminator
	return g.MainToken.Type != ILLEGAL && len(g.Terminators) > 0
}
