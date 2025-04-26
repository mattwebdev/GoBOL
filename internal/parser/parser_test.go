package parser

import (
	"strings"
	"testing"

	"github.com/mattwebdev/gobol/internal/ast"
	"github.com/mattwebdev/gobol/internal/lexer"
)

func TestIdentificationDivision(t *testing.T) {
	input := `IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROG.
       AUTHOR. JOHN DOE.
       DATE-WRITTEN. 2024-03-20.`

	l := lexer.New(input, false)
	p := New(l)

	program := p.ParseProgram()
	if program == nil {
		t.Fatal("ParseProgram() returned nil")
	}

	if program.IdentificationDivision == nil {
		t.Fatal("program.IdentificationDivision is nil")
	}

	tests := []struct {
		got      string
		expected string
	}{
		{program.IdentificationDivision.ProgramID.Name, "TEST-PROG"},
		{program.IdentificationDivision.Author, "JOHN DOE"},
		{program.IdentificationDivision.Written, "2024-03-20"},
	}

	for i, tt := range tests {
		if tt.got != tt.expected {
			t.Errorf("test[%d] - got=%q, want=%q", i, tt.got, tt.expected)
		}
	}
}

func TestDataDivision(t *testing.T) {
	input := `DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CUSTOMER-RECORD.
          05 CUST-ID PIC 9(5).
          05 CUST-NAME PIC X(20).
          05 CUST-BALANCE PIC 9(7)V99.`

	l := lexer.New(input, false)
	p := New(l)

	program := p.ParseProgram()
	if program == nil {
		t.Fatal("ParseProgram() returned nil")
	}

	if program.DataDivision == nil {
		t.Fatal("program.DataDivision is nil")
	}

	if program.DataDivision.WorkingStorage == nil {
		t.Fatal("program.DataDivision.WorkingStorage is nil")
	}

	records := program.DataDivision.WorkingStorage.Records
	if len(records) != 3 {
		t.Fatalf("expected 3 records, got %d", len(records))
	}

	tests := []struct {
		name       string
		level      string
		picClause  string
		decimalPos int
	}{
		{"CUSTOMER-RECORD", "01", "", 0},
		{"CUST-ID", "05", "9(5)", 0},
		{"CUST-NAME", "05", "X(20)", 0},
		{"CUST-BALANCE", "05", "9(7)V99", 7},
	}

	for i, tt := range tests {
		record := records[i]
		if record.Name != tt.name {
			t.Errorf("record[%d].Name = %q, want %q", i, record.Name, tt.name)
		}
		if record.LevelNumber != tt.level {
			t.Errorf("record[%d].LevelNumber = %q, want %q", i, record.LevelNumber, tt.level)
		}
		if record.Picture != nil && record.Picture.Type != tt.picClause {
			t.Errorf("record[%d].Picture.Type = %q, want %q", i, record.Picture.Type, tt.picClause)
		}
		if record.Picture != nil && record.Picture.DecimalPos != tt.decimalPos {
			t.Errorf("record[%d].Picture.DecimalPos = %d, want %d", i, record.Picture.DecimalPos, tt.decimalPos)
		}
	}
}

func TestProcedureDivision(t *testing.T) {
	input := `PROCEDURE DIVISION.
           MOVE 100 TO TOTAL.
           IF TOTAL > 50
              DISPLAY "Greater than 50"
              MOVE SPACES TO RESULT
           ELSE
              DISPLAY "Less or equal to 50"
           END-IF.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 10
              DISPLAY IDX
           END-PERFORM.`

	l := lexer.New(input, false)
	p := New(l)

	program := p.ParseProgram()
	if program == nil {
		t.Fatal("ParseProgram() returned nil")
	}

	if program.ProcedureDivision == nil {
		t.Fatal("program.ProcedureDivision is nil")
	}

	statements := program.ProcedureDivision.Statements
	if len(statements) != 3 {
		t.Fatalf("expected 3 statements, got %d", len(statements))
	}

	// Test MOVE statement
	moveStmt, ok := statements[0].(*ast.MoveStatement)
	if !ok {
		t.Fatalf("statements[0] not *ast.MoveStatement. got=%T", statements[0])
	}
	if len(moveStmt.To) != 1 {
		t.Errorf("moveStmt.To has wrong length. got=%d", len(moveStmt.To))
	}

	// Test IF statement
	ifStmt, ok := statements[1].(*ast.IfStatement)
	if !ok {
		t.Fatalf("statements[1] not *ast.IfStatement. got=%T", statements[1])
	}
	if len(ifStmt.Consequence) != 2 {
		t.Errorf("ifStmt.Consequence has wrong length. got=%d", len(ifStmt.Consequence))
	}
	if len(ifStmt.Alternative) != 1 {
		t.Errorf("ifStmt.Alternative has wrong length. got=%d", len(ifStmt.Alternative))
	}

	// Test PERFORM statement
	performStmt, ok := statements[2].(*ast.PerformStatement)
	if !ok {
		t.Fatalf("statements[2] not *ast.PerformStatement. got=%T", statements[2])
	}
	if performStmt.Varying == nil {
		t.Fatal("performStmt.Varying is nil")
	}
	if len(performStmt.Body) != 1 {
		t.Errorf("performStmt.Body has wrong length. got=%d", len(performStmt.Body))
	}
}

func TestParseErrors(t *testing.T) {
	tests := []struct {
		input       string
		errorCount  int
		errorPrefix string
	}{
		{
			`IDENTIFICATION DIVISION.`, // Missing PROGRAM-ID
			1,
			"missing PROGRAM-ID",
		},
		{
			`PROCEDURE DIVISION.
             COMPUTE RESULT = "abc".`, // Type mismatch
			1,
			"COMPUTE value must be numeric",
		},
		{
			`DATA DIVISION.
             WORKING-STORAGE SECTION.
             01 NUM1 PIC ABC.`, // Invalid PIC clause
			1,
			"invalid picture clause",
		},
	}

	for i, tt := range tests {
		l := lexer.New(tt.input, false)
		p := New(l)

		_ = p.ParseProgram()
		errors := p.Errors()

		if len(errors) != tt.errorCount {
			t.Errorf("test[%d] - wrong number of errors. expected=%d, got=%d",
				i, tt.errorCount, len(errors))
			continue
		}

		if len(errors) > 0 && !strings.HasPrefix(errors[0], tt.errorPrefix) {
			t.Errorf("test[%d] - wrong error prefix. expected=%q, got=%q",
				i, tt.errorPrefix, errors[0])
		}
	}
}
