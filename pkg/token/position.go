package token

import "fmt"

// Area represents a COBOL source code area (A or B)
type Area rune

const (
	AreaA Area = 'A'
	AreaB Area = 'B'
)

// Position represents a position in the source code with COBOL-specific information
type Position struct {
	Filename     string // Source file name
	Line         int    // 1-based line number
	Column       int    // 1-based column number
	Offset       int    // 0-based byte offset in source
	Area         Area   // COBOL Area A or B
	SeqNumber    string // Sequence number (columns 1-6)
	Continuation bool   // Whether this is a continuation line
}

// NewPosition creates a new Position instance
func NewPosition(filename string, line, column, offset int, area Area) Position {
	return Position{
		Filename: filename,
		Line:     line,
		Column:   column,
		Offset:   offset,
		Area:     area,
	}
}

// WithSequenceNumber adds sequence number information to the position
func (p Position) WithSequenceNumber(seq string) Position {
	p.SeqNumber = seq
	return p
}

// WithContinuation marks the position as a continuation line
func (p Position) WithContinuation(cont bool) Position {
	p.Continuation = cont
	return p
}

// String returns a string representation of the position
func (p Position) String() string {
	areaStr := ""
	if p.Area != 0 {
		areaStr = fmt.Sprintf(", Area %c", p.Area)
	}

	seqStr := ""
	if p.SeqNumber != "" {
		seqStr = fmt.Sprintf(", Seq: %s", p.SeqNumber)
	}

	contStr := ""
	if p.Continuation {
		contStr = " (continuation)"
	}

	return fmt.Sprintf("%s:line %d, column %d%s%s%s",
		p.Filename, p.Line, p.Column, areaStr, seqStr, contStr)
}
