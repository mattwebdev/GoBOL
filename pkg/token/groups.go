package token

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
