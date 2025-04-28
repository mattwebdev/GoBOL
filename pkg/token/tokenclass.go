package token

// TokenClass represents the classification of a token
type TokenClass int

const (
	// Basic token classes
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
	CLASS_DATATYPE              // Data type declarations

	// Additional token classes for specific contexts
	CLASS_SCOPE     // Scope terminators (END-IF, etc)
	CLASS_CONDITION // Condition keywords (ON, NOT, etc)
	CLASS_FILE      // File-related keywords
)

// IsValidClass checks if a TokenClass value is valid
func IsValidClass(class TokenClass) bool {
	return class >= CLASS_UNKNOWN && class <= CLASS_FILE
}

// String returns the string representation of a TokenClass
func (c TokenClass) String() string {
	switch c {
	case CLASS_UNKNOWN:
		return "UNKNOWN"
	case CLASS_VERB:
		return "VERB"
	case CLASS_KEYWORD:
		return "KEYWORD"
	case CLASS_MODIFIER:
		return "MODIFIER"
	case CLASS_IDENTIFIER:
		return "IDENTIFIER"
	case CLASS_LITERAL:
		return "LITERAL"
	case CLASS_OPERATOR:
		return "OPERATOR"
	case CLASS_SEPARATOR:
		return "SEPARATOR"
	case CLASS_DIVISION:
		return "DIVISION"
	case CLASS_SECTION:
		return "SECTION"
	case CLASS_DATATYPE:
		return "DATATYPE"
	case CLASS_SCOPE:
		return "SCOPE"
	case CLASS_CONDITION:
		return "CONDITION"
	case CLASS_FILE:
		return "FILE"
	default:
		return "INVALID"
	}
}
