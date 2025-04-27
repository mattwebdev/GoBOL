package token

import (
	"testing"
)

func TestNewTokenGroup(t *testing.T) {
	mainToken := TokenInfo{
		Type:    MOVE,
		Literal: "MOVE",
		Pos:     Position{Line: 1, Column: 1},
	}

	group := NewTokenGroup(mainToken)

	if group.MainToken != mainToken {
		t.Errorf("NewTokenGroup() MainToken = %v, want %v", group.MainToken, mainToken)
	}
	if len(group.Modifiers) != 0 {
		t.Errorf("NewTokenGroup() Modifiers length = %v, want 0", len(group.Modifiers))
	}
	if len(group.Parameters) != 0 {
		t.Errorf("NewTokenGroup() Parameters length = %v, want 0", len(group.Parameters))
	}
	if len(group.Terminators) != 0 {
		t.Errorf("NewTokenGroup() Terminators length = %v, want 0", len(group.Terminators))
	}
}

func TestTokenGroup_AddModifier(t *testing.T) {
	mainToken := TokenInfo{
		Type:    MOVE,
		Literal: "MOVE",
		Pos:     Position{Line: 1, Column: 1},
	}
	modifier := TokenInfo{
		Type:    CORRESPONDING,
		Literal: "CORRESPONDING",
		Pos:     Position{Line: 1, Column: 6},
	}

	group := NewTokenGroup(mainToken)
	group.AddModifier(modifier)

	if len(group.Modifiers) != 1 {
		t.Errorf("AddModifier() Modifiers length = %v, want 1", len(group.Modifiers))
	}
	if group.Modifiers[0] != modifier {
		t.Errorf("AddModifier() Modifier = %v, want %v", group.Modifiers[0], modifier)
	}
}

func TestTokenGroup_AddParameter(t *testing.T) {
	mainToken := TokenInfo{
		Type:    MOVE,
		Literal: "MOVE",
		Pos:     Position{Line: 1, Column: 1},
	}
	param := TokenInfo{
		Type:    IDENTIFIER,
		Literal: "MyVar",
		Pos:     Position{Line: 1, Column: 6},
	}

	group := NewTokenGroup(mainToken)
	group.AddParameter(param)

	if len(group.Parameters) != 1 {
		t.Errorf("AddParameter() Parameters length = %v, want 1", len(group.Parameters))
	}
	if group.Parameters[0] != param {
		t.Errorf("AddParameter() Parameter = %v, want %v", group.Parameters[0], param)
	}
}

func TestTokenGroup_AddTerminator(t *testing.T) {
	mainToken := TokenInfo{
		Type:    MOVE,
		Literal: "MOVE",
		Pos:     Position{Line: 1, Column: 1},
	}
	terminator := TokenInfo{
		Type:    OP_PERIOD,
		Literal: ".",
		Pos:     Position{Line: 1, Column: 20},
	}

	group := NewTokenGroup(mainToken)
	group.AddTerminator(terminator)

	if len(group.Terminators) != 1 {
		t.Errorf("AddTerminator() Terminators length = %v, want 1", len(group.Terminators))
	}
	if group.Terminators[0] != terminator {
		t.Errorf("AddTerminator() Terminator = %v, want %v", group.Terminators[0], terminator)
	}
}

func TestTokenGroup_IsComplete(t *testing.T) {
	tests := []struct {
		name     string
		group    *TokenGroup
		expected bool
	}{
		{
			name: "Complete group with terminator",
			group: &TokenGroup{
				MainToken: TokenInfo{Type: MOVE, Literal: "MOVE"},
				Terminators: []TokenInfo{
					{Type: OP_PERIOD, Literal: "."},
				},
			},
			expected: true,
		},
		{
			name: "Incomplete group - no terminator",
			group: &TokenGroup{
				MainToken: TokenInfo{Type: MOVE, Literal: "MOVE"},
			},
			expected: false,
		},
		{
			name: "Incomplete group - illegal main token",
			group: &TokenGroup{
				MainToken: TokenInfo{Type: ILLEGAL},
				Terminators: []TokenInfo{
					{Type: OP_PERIOD, Literal: "."},
				},
			},
			expected: false,
		},
		{
			name: "Complete group with modifiers and parameters",
			group: &TokenGroup{
				MainToken: TokenInfo{Type: MOVE, Literal: "MOVE"},
				Modifiers: []TokenInfo{
					{Type: CORRESPONDING, Literal: "CORRESPONDING"},
				},
				Parameters: []TokenInfo{
					{Type: IDENTIFIER, Literal: "MyVar"},
					{Type: IDENTIFIER, Literal: "YourVar"},
				},
				Terminators: []TokenInfo{
					{Type: OP_PERIOD, Literal: "."},
				},
			},
			expected: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.group.IsComplete(); got != tt.expected {
				t.Errorf("IsComplete() = %v, want %v", got, tt.expected)
			}
		})
	}
}

func TestTokenClass_Values(t *testing.T) {
	tests := []struct {
		name  string
		class TokenClass
	}{
		{"Unknown", CLASS_UNKNOWN},
		{"Verb", CLASS_VERB},
		{"Keyword", CLASS_KEYWORD},
		{"Modifier", CLASS_MODIFIER},
		{"Identifier", CLASS_IDENTIFIER},
		{"Literal", CLASS_LITERAL},
		{"Operator", CLASS_OPERATOR},
		{"Separator", CLASS_SEPARATOR},
		{"Division", CLASS_DIVISION},
		{"Section", CLASS_SECTION},
	}

	seen := make(map[TokenClass]string)
	for _, tt := range tests {
		if existing, exists := seen[tt.class]; exists {
			t.Errorf("TokenClass value collision between %s and %s", tt.name, existing)
		}
		seen[tt.class] = tt.name
	}
}
