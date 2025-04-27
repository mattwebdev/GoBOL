package token

// LiteralType represents the specific type of a literal
type LiteralType int

const (
	// Literal type constants
	LIT_UNKNOWN      LiteralType = iota
	LIT_NUMERIC                  // Regular numeric literal
	LIT_DECIMAL                  // Decimal literal
	LIT_ALPHANUMERIC             // Quoted string literal
	LIT_HEXADECIMAL              // Hex literal
	LIT_BOOLEAN                  // TRUE/FALSE
	LIT_FIGURATIVE               // ZERO, SPACE, HIGH-VALUE etc.
)

// Figurative constant tokens start at 300
const (
	ZERO   Token = iota + 300
	ZEROS        // Alias for ZERO
	ZEROES       // Alias for ZERO
	SPACE
	SPACES // Alias for SPACE
	HIGH_VALUE
	HIGH_VALUES // Alias for HIGH_VALUE
	LOW_VALUE
	LOW_VALUES // Alias for LOW_VALUE
	QUOTE
	QUOTES // Alias for QUOTE
	NULL
	NULLS // Alias for NULL
	ALL   // Used with other figurative constants
)

// LiteralInfo contains metadata about a literal
type LiteralInfo struct {
	Type        LiteralType
	IsNumeric   bool
	IsSigned    bool
	Length      int  // -1 for variable length
	MaxLength   int  // Maximum allowed length
	IsRepeating bool // Whether it can be repeated (like ALL)
}

// FigurativeConstantInfo contains information about figurative constants
type FigurativeConstantInfo struct {
	Token     Token
	Aliases   []Token
	Type      LiteralType
	Value     string // The actual value it represents
	CanUseALL bool   // Whether it can be used with ALL
}

// Figurative constant definitions
var figurativeConstants = map[Token]FigurativeConstantInfo{
	ZERO: {
		Token:     ZERO,
		Aliases:   []Token{ZEROS, ZEROES},
		Type:      LIT_FIGURATIVE,
		Value:     "0",
		CanUseALL: true,
	},
	SPACE: {
		Token:     SPACE,
		Aliases:   []Token{SPACES},
		Type:      LIT_FIGURATIVE,
		Value:     " ",
		CanUseALL: true,
	},
	HIGH_VALUE: {
		Token:     HIGH_VALUE,
		Aliases:   []Token{HIGH_VALUES},
		Type:      LIT_FIGURATIVE,
		Value:     "\xFF",
		CanUseALL: true,
	},
	LOW_VALUE: {
		Token:     LOW_VALUE,
		Aliases:   []Token{LOW_VALUES},
		Type:      LIT_FIGURATIVE,
		Value:     "\x00",
		CanUseALL: true,
	},
	// Add more as needed...
}

// IsFigurativeConstant checks if a token is a figurative constant
func IsFigurativeConstant(t Token) bool {
	_, ok := figurativeConstants[t]
	return ok
}

// GetFigurativeConstantInfo returns info about a figurative constant
func GetFigurativeConstantInfo(t Token) (FigurativeConstantInfo, bool) {
	info, ok := figurativeConstants[t]
	return info, ok
}

// IsLiteral checks if a token represents any kind of literal
func IsLiteral(t Token) bool {
	return t == STRING_LIT || t == NUMBER_LIT || IsFigurativeConstant(t)
}
