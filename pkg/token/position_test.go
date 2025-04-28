package token

import (
	"testing"
)

func TestNewPosition(t *testing.T) {
	tests := []struct {
		name     string
		filename string
		line     int
		column   int
		offset   int
		area     Area
		expected Position
	}{
		{
			name:     "Create position in Area A",
			filename: "test.cbl",
			line:     1,
			column:   8,
			offset:   7,
			area:     AreaA,
			expected: Position{
				Filename: "test.cbl",
				Line:     1,
				Column:   8,
				Offset:   7,
				Area:     AreaA,
			},
		},
		{
			name:     "Create position in Area B",
			filename: "test.cbl",
			line:     2,
			column:   12,
			offset:   20,
			area:     AreaB,
			expected: Position{
				Filename: "test.cbl",
				Line:     2,
				Column:   12,
				Offset:   20,
				Area:     AreaB,
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := NewPosition(tt.filename, tt.line, tt.column, tt.offset, tt.area)
			if got != tt.expected {
				t.Errorf("NewPosition() = %v, want %v", got, tt.expected)
			}
		})
	}
}

func TestPosition_WithSequenceNumber(t *testing.T) {
	tests := []struct {
		name     string
		pos      Position
		seq      string
		expected Position
	}{
		{
			name: "Add sequence number",
			pos: Position{
				Filename: "test.cbl",
				Line:     1,
				Column:   8,
				Area:     AreaA,
			},
			seq: "000100",
			expected: Position{
				Filename:  "test.cbl",
				Line:      1,
				Column:    8,
				Area:      AreaA,
				SeqNumber: "000100",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.pos.WithSequenceNumber(tt.seq)
			if got != tt.expected {
				t.Errorf("Position.WithSequenceNumber() = %v, want %v", got, tt.expected)
			}
		})
	}
}

func TestPosition_WithContinuation(t *testing.T) {
	tests := []struct {
		name     string
		pos      Position
		cont     bool
		expected Position
	}{
		{
			name: "Mark as continuation line",
			pos: Position{
				Filename: "test.cbl",
				Line:     1,
				Column:   8,
				Area:     AreaA,
			},
			cont: true,
			expected: Position{
				Filename:     "test.cbl",
				Line:         1,
				Column:       8,
				Area:         AreaA,
				Continuation: true,
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.pos.WithContinuation(tt.cont)
			if got != tt.expected {
				t.Errorf("Position.WithContinuation() = %v, want %v", got, tt.expected)
			}
		})
	}
}

func TestPosition_String(t *testing.T) {
	tests := []struct {
		name     string
		pos      Position
		expected string
	}{
		{
			name: "Basic position",
			pos: Position{
				Filename: "test.cbl",
				Line:     1,
				Column:   8,
			},
			expected: "test.cbl:line 1, column 8",
		},
		{
			name: "Position with Area",
			pos: Position{
				Filename: "test.cbl",
				Line:     1,
				Column:   8,
				Area:     AreaA,
			},
			expected: "test.cbl:line 1, column 8, Area A",
		},
		{
			name: "Position with sequence number",
			pos: Position{
				Filename:  "test.cbl",
				Line:      1,
				Column:    8,
				Area:      AreaA,
				SeqNumber: "000100",
			},
			expected: "test.cbl:line 1, column 8, Area A, Seq: 000100",
		},
		{
			name: "Position with continuation",
			pos: Position{
				Filename:     "test.cbl",
				Line:         1,
				Column:       8,
				Area:         AreaA,
				SeqNumber:    "000100",
				Continuation: true,
			},
			expected: "test.cbl:line 1, column 8, Area A, Seq: 000100 (continuation)",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.pos.String(); got != tt.expected {
				t.Errorf("Position.String() = %v, want %v", got, tt.expected)
			}
		})
	}
}
