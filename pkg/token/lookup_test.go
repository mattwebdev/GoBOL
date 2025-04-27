package token

import (
	"strings"
	"testing"
)

func TestLookupToken(t *testing.T) {
	tests := []struct {
		input    string
		expected Token
	}{
		// Level numbers
		{"01", LEVEL_NUMBER},
		{"1", LEVEL_NUMBER},
		{"77", LEVEL_NUMBER},
		{"88", LEVEL_NUMBER},
		{"05", LEVEL_NUMBER},
		{"49", LEVEL_NUMBER},
		{"50", IDENTIFIER}, // Not a valid level number
		{"00", ILLEGAL},    // Not a valid level number

		// Numeric literals
		{"42", NUMBER_LIT},
		{"+42", NUMBER_LIT},
		{"-42", NUMBER_LIT},
		{"3.14", NUMBER_LIT},
		{"+3.14", NUMBER_LIT},
		{"-3.14", NUMBER_LIT},
		{".", ILLEGAL},       // Not a valid number
		{"+", ILLEGAL},       // Not a valid number
		{"3.14.15", ILLEGAL}, // Not a valid number

		// String literals
		{`"Hello"`, STRING_LIT},
		{`'World'`, STRING_LIT},
		{`"`, ILLEGAL},         // Incomplete string
		{`'`, ILLEGAL},         // Incomplete string
		{`"Hello`, ILLEGAL},    // Unterminated string
		{`Hello"`, IDENTIFIER}, // Not a string literal

		// Keywords and verbs (case insensitive)
		{"MOVE", MOVE},
		{"move", MOVE},
		{"Move", MOVE},
		{"CORRESPONDING", CORRESPONDING},
		{"CORR", CORRESPONDING}, // Alias

		// Operators
		{"+", OP_PLUS},
		{"-", OP_MINUS},
		{"*", OP_MULTIPLY},
		{"/", OP_DIVIDE},
		{"=", OP_EQUAL_TO},
		{"EQUAL", OP_EQUAL_TO},
		{"EQUAL TO", OP_EQUAL_TO},

		// Divisions
		{"IDENTIFICATION DIVISION", IDENTIFICATION_DIVISION},
		{"DATA DIVISION", DATA_DIVISION},
		{"PROCEDURE DIVISION", PROCEDURE_DIVISION},

		// Figurative constants
		{"ZERO", ZERO},
		{"ZEROS", ZERO},  // Alias
		{"ZEROES", ZERO}, // Alias
		{"SPACE", SPACE},
		{"SPACES", SPACE}, // Alias
		{"HIGH-VALUE", HIGH_VALUE},
		{"HIGH-VALUES", HIGH_VALUE}, // Alias

		// Valid identifiers
		{"MyVar", IDENTIFIER},
		{"my-var", IDENTIFIER},
		{"VAR1", IDENTIFIER},
		{"A", IDENTIFIER},
		{"-START", IDENTIFIER},

		// Invalid identifiers
		{"1VAR", NUMBER_LIT},               // Starts with number
		{"My Var", ILLEGAL},                // Contains space
		{"My_Var", ILLEGAL},                // Contains underscore
		{"", ILLEGAL},                      // Empty string
		{strings.Repeat("A", 31), ILLEGAL}, // Too long (> 30 chars)
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			if got := LookupToken(tt.input); got != tt.expected {
				t.Errorf("LookupToken(%q) = %v, want %v", tt.input, got, tt.expected)
			}
		})
	}
}

func TestIsLevelNumber(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{"01", true},
		{"1", true},
		{"77", true},
		{"88", true},
		{"05", true},
		{"49", true},
		{"50", false},
		{"00", false},
		{"100", false},
		{"0", false},
		{"abc", false},
		{"", false},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			if got := IsLevelNumber(tt.input); got != tt.expected {
				t.Errorf("IsLevelNumber(%q) = %v, want %v", tt.input, got, tt.expected)
			}
		})
	}
}

func TestIsNumeric(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{"42", true},
		{"+42", true},
		{"-42", true},
		{"3.14", true},
		{"+3.14", true},
		{"-3.14", true},
		{"0", true},
		{"+0", true},
		{"-0", true},
		{".", false},
		{"+", false},
		{"-", false},
		{"3.14.15", false},
		{"abc", false},
		{"", false},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			if got := IsNumeric(tt.input); got != tt.expected {
				t.Errorf("IsNumeric(%q) = %v, want %v", tt.input, got, tt.expected)
			}
		})
	}
}

func TestIsStringLiteral(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{`"Hello"`, true},
		{`'World'`, true},
		{`"Hello, World!"`, true},
		{`'Hello, World!'`, true},
		{`""`, true},
		{`''`, true},
		{`"`, false},
		{`'`, false},
		{`"Hello`, false},
		{`Hello"`, false},
		{`Hello`, false},
		{``, false},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			if got := IsStringLiteral(tt.input); got != tt.expected {
				t.Errorf("IsStringLiteral(%q) = %v, want %v", tt.input, got, tt.expected)
			}
		})
	}
}

func TestIsValidIdentifier(t *testing.T) {
	tests := []struct {
		input    string
		expected bool
	}{
		{"MyVar", true},
		{"my-var", true},
		{"VAR1", true},
		{"A", true},
		{"-START", true},
		{"HELLO-WORLD", true},
		{"X1Y2Z3", true},
		{"1VAR", false},                  // Starts with number
		{"My Var", false},                // Contains space
		{"My_Var", false},                // Contains underscore
		{"", false},                      // Empty string
		{strings.Repeat("A", 31), false}, // Too long (> 30 chars)
		{"My@Var", false},                // Contains special character
		{"123", false},                   // All numbers
		{"--", false},                    // Just hyphens
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			if got := IsValidIdentifier(tt.input); got != tt.expected {
				t.Errorf("IsValidIdentifier(%q) = %v, want %v", tt.input, got, tt.expected)
			}
		})
	}
}

func TestHelperFunctions(t *testing.T) {
	// Test isLetter
	letters := []struct {
		input    rune
		expected bool
	}{
		{'A', true},
		{'Z', true},
		{'a', true},
		{'z', true},
		{'0', false},
		{'9', false},
		{'-', false},
		{'@', false},
		{' ', false},
	}

	for _, tt := range letters {
		t.Run(string(tt.input), func(t *testing.T) {
			if got := isLetter(tt.input); got != tt.expected {
				t.Errorf("isLetter(%q) = %v, want %v", tt.input, got, tt.expected)
			}
		})
	}

	// Test isDigit
	digits := []struct {
		input    rune
		expected bool
	}{
		{'0', true},
		{'9', true},
		{'A', false},
		{'Z', false},
		{'a', false},
		{'z', false},
		{'-', false},
		{'@', false},
		{' ', false},
	}

	for _, tt := range digits {
		t.Run(string(tt.input), func(t *testing.T) {
			if got := isDigit(tt.input); got != tt.expected {
				t.Errorf("isDigit(%q) = %v, want %v", tt.input, got, tt.expected)
			}
		})
	}
}
