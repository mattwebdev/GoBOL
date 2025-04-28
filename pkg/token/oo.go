package token

// OOInfo represents information about an object-oriented token
type OOInfo struct {
	Description string
	Context     []string
	Usage       string
}

// OOTokens maps OO tokens to their information
var OOTokens = map[Token]OOInfo{
	CLASS_DEFINITION: {
		Description: "Defines a class",
		Context:     []string{"CLASS", "CLASS-ID"},
		Usage:       "CLASS-ID class-name",
	},
	OO_OBJECT: {
		Description: "Object instance",
		Context:     []string{"OBJECT", "OBJECT-REFERENCE"},
		Usage:       "OBJECT object-name",
	},
	OO_METHOD: {
		Description: "Class method",
		Context:     []string{"METHOD", "METHOD-ID"},
		Usage:       "METHOD-ID method-name",
	},
	OO_PROPERTY: {
		Description: "Class property",
		Context:     []string{"PROPERTY", "PROPERTY-ID"},
		Usage:       "PROPERTY-ID property-name",
	},
	OO_INHERITS: {
		Description: "Class inheritance",
		Context:     []string{"INHERITS"},
		Usage:       "INHERITS class-name",
	},
	OO_INTERFACE: {
		Description: "Interface definition",
		Context:     []string{"INTERFACE", "INTERFACE-ID"},
		Usage:       "INTERFACE-ID interface-name",
	},
	OO_IMPLEMENTS: {
		Description: "Interface implementation",
		Context:     []string{"IMPLEMENTS"},
		Usage:       "IMPLEMENTS interface-name",
	},
	OO_NEW: {
		Description: "Object instantiation",
		Context:     []string{"NEW"},
		Usage:       "NEW object-name",
	},
	OO_SUPER: {
		Description: "Parent class reference",
		Context:     []string{"SUPER"},
		Usage:       "SUPER::method-name",
	},
	OO_THIS: {
		Description: "Current object reference",
		Context:     []string{"THIS"},
		Usage:       "THIS::method-name",
	},
	OO_EVENT: {
		Description: "Event definition",
		Context:     []string{"EVENT", "EVENT-ID"},
		Usage:       "EVENT-ID event-name",
	},
	OO_RAISE: {
		Description: "Event raising",
		Context:     []string{"RAISE"},
		Usage:       "RAISE event-name",
	},
	OO_HANDLE: {
		Description: "Event handling",
		Context:     []string{"HANDLE"},
		Usage:       "HANDLE event-name",
	},
}

// IsOOToken checks if a token represents an OO token
func IsOOToken(t Token) bool {
	_, exists := OOTokens[t]
	return exists
}

// GetOOInfo returns information about an OO token
func GetOOInfo(t Token) (OOInfo, bool) {
	info, exists := OOTokens[t]
	return info, exists
}

// IsValidOOContext checks if an OO token is valid in its current context
func IsValidOOContext(t Token, context []string) bool {
	info, exists := OOTokens[t]
	if !exists {
		return false
	}

	for _, allowedContext := range info.Context {
		for _, currentContext := range context {
			if allowedContext == currentContext {
				return true
			}
		}
	}
	return false
}
