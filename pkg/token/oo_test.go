package token

import "testing"

func TestIsValidOOContext(t *testing.T) {
	tests := []struct {
		token    Token
		context  []string
		expected bool
	}{
		{OO_OBJECT, []string{"OBJECT"}, true},
		{OO_OBJECT, []string{"INVALID"}, false},
		{ILLEGAL, []string{"OBJECT"}, false},
	}

	for _, test := range tests {
		result := IsValidOOContext(test.token, test.context)
		if result != test.expected {
			t.Errorf("IsValidOOContext(%v, %v) = %v, want %v", test.token, test.context, result, test.expected)
		}
	}
}
