package analyzer

import (
	"fmt"

	"github.com/mattwebdev/gobol/internal/ast"
)

// Type represents a COBOL data type
type Type int

const (
	TYPE_UNKNOWN Type = iota
	TYPE_NUMERIC
	TYPE_ALPHANUMERIC
	TYPE_PACKED_DECIMAL
	TYPE_BINARY
)

// Symbol represents a variable or identifier in the symbol table
type Symbol struct {
	Name       string
	Type       Type
	Level      int    // COBOL level number
	Size       int    // Size in bytes
	DecimalPos int    // Position of decimal point for numeric types
	IsEdited   bool   // Whether this is an edited field
	Picture    string // Original PIC clause
	IsFileDesc bool   // Whether this is a file descriptor
}

// Scope represents a symbol table for a specific scope
type Scope struct {
	symbols    map[string]*Symbol
	parent     *Scope
	children   []*Scope
	isFileDesc bool // Whether this scope is for a file description
}

// Analyzer performs semantic analysis on the AST
type Analyzer struct {
	currentScope *Scope
	errors       []string
}

// New creates a new semantic analyzer
func New() *Analyzer {
	return &Analyzer{
		currentScope: newScope(nil),
	}
}

// newScope creates a new scope with an optional parent
func newScope(parent *Scope) *Scope {
	s := &Scope{
		symbols:  make(map[string]*Symbol),
		parent:   parent,
		children: make([]*Scope, 0),
	}
	if parent != nil {
		parent.children = append(parent.children, s)
	}
	return s
}

// Analyze performs semantic analysis on a COBOL program
func (a *Analyzer) Analyze(program *ast.Program) error {
	// Analyze each division in order
	if err := a.analyzeIdentificationDivision(program.IdentificationDivision); err != nil {
		return err
	}
	if err := a.analyzeEnvironmentDivision(program.EnvironmentDivision); err != nil {
		return err
	}
	if err := a.analyzeDataDivision(program.DataDivision); err != nil {
		return err
	}
	if err := a.analyzeProcedureDivision(program.ProcedureDivision); err != nil {
		return err
	}
	return nil
}

// analyzeIdentificationDivision checks the IDENTIFICATION DIVISION
func (a *Analyzer) analyzeIdentificationDivision(div *ast.IdentificationDivision) error {
	if div == nil {
		return fmt.Errorf("missing IDENTIFICATION DIVISION")
	}
	if div.ProgramID == nil || div.ProgramID.Name == "" {
		return fmt.Errorf("missing PROGRAM-ID")
	}
	return nil
}

// analyzeDataDivision analyzes the DATA DIVISION
func (a *Analyzer) analyzeDataDivision(div *ast.DataDivision) error {
	if div == nil {
		return nil // DATA DIVISION is optional
	}

	// Create new scope for DATA DIVISION
	prevScope := a.currentScope
	a.currentScope = newScope(prevScope)
	defer func() { a.currentScope = prevScope }()

	// Analyze FILE SECTION
	if div.FileSection != nil {
		if err := a.analyzeFileSection(div.FileSection); err != nil {
			return err
		}
	}

	// Analyze WORKING-STORAGE SECTION
	if div.WorkingStorage != nil {
		if err := a.analyzeWorkingStorageSection(div.WorkingStorage); err != nil {
			return err
		}
	}

	return nil
}

// analyzeDataRecord analyzes a data record entry
func (a *Analyzer) analyzeDataRecord(record *ast.DataRecord) error {
	if record == nil {
		return nil
	}

	// Parse level number
	level := 0
	fmt.Sscanf(record.LevelNumber, "%d", &level)

	// Create symbol for this record
	symbol := &Symbol{
		Name:  record.Name,
		Level: level,
	}

	// Analyze PIC clause if present
	if record.Picture != nil {
		typ, size, decPos, isEdited := a.analyzePicture(record.Picture)
		symbol.Type = typ
		symbol.Size = size
		symbol.DecimalPos = decPos
		symbol.IsEdited = isEdited
		symbol.Picture = record.Picture.Type
	}

	// Add symbol to current scope
	if record.Name != "" {
		if existing := a.currentScope.symbols[record.Name]; existing != nil {
			return fmt.Errorf("duplicate definition of %s", record.Name)
		}
		a.currentScope.symbols[record.Name] = symbol
	}

	return nil
}

// analyzePicture analyzes a PIC clause and returns type information
func (a *Analyzer) analyzePicture(pic *ast.PictureClause) (Type, int, int, bool) {
	switch pic.Type {
	case "9":
		return TYPE_NUMERIC, pic.Length, pic.DecimalPos, pic.IsEdited
	case "X":
		return TYPE_ALPHANUMERIC, pic.Length, 0, pic.IsEdited
	default:
		return TYPE_UNKNOWN, 0, 0, false
	}
}

// analyzeProcedureDivision analyzes the PROCEDURE DIVISION
func (a *Analyzer) analyzeProcedureDivision(div *ast.ProcedureDivision) error {
	if div == nil {
		return fmt.Errorf("missing PROCEDURE DIVISION")
	}

	// Create new scope for PROCEDURE DIVISION
	prevScope := a.currentScope
	a.currentScope = newScope(prevScope)
	defer func() { a.currentScope = prevScope }()

	// Analyze each statement
	for _, stmt := range div.Statements {
		if err := a.analyzeStatement(stmt); err != nil {
			return err
		}
	}

	return nil
}

// analyzeStatement performs type checking on a statement
func (a *Analyzer) analyzeStatement(stmt ast.Statement) error {
	switch s := stmt.(type) {
	case *ast.MoveStatement:
		return a.analyzeMoveStatement(s)
	case *ast.ComputeStatement:
		return a.analyzeComputeStatement(s)
	case *ast.IfStatement:
		return a.analyzeIfStatement(s)
	case *ast.PerformStatement:
		return a.analyzePerformStatement(s)
	case *ast.DisplayStatement:
		return a.analyzeDisplayStatement(s)
	default:
		return fmt.Errorf("unknown statement type")
	}
}

// analyzeMoveStatement checks type compatibility in MOVE statements
func (a *Analyzer) analyzeMoveStatement(stmt *ast.MoveStatement) error {
	// Get source type
	sourceType := a.getExpressionType(stmt.Value)

	// Check each target
	for _, target := range stmt.To {
		targetType := a.getExpressionType(target)
		if !a.isAssignmentCompatible(sourceType, targetType) {
			return fmt.Errorf("incompatible types in MOVE statement")
		}
	}

	return nil
}

// getExpressionType determines the type of an expression
func (a *Analyzer) getExpressionType(expr ast.Expression) Type {
	switch e := expr.(type) {
	case *ast.Identifier:
		if sym := a.lookupSymbol(e.Value); sym != nil {
			return sym.Type
		}
		return TYPE_UNKNOWN
	case *ast.NumericLiteral:
		return TYPE_NUMERIC
	case *ast.StringLiteral:
		return TYPE_ALPHANUMERIC
	default:
		return TYPE_UNKNOWN
	}
}

// lookupSymbol searches for a symbol in the current scope and its parents
func (a *Analyzer) lookupSymbol(name string) *Symbol {
	for scope := a.currentScope; scope != nil; scope = scope.parent {
		if sym := scope.symbols[name]; sym != nil {
			return sym
		}
	}
	return nil
}

// isAssignmentCompatible checks if two types are compatible for assignment
func (a *Analyzer) isAssignmentCompatible(source, target Type) bool {
	if source == target {
		return true
	}
	// Add more type compatibility rules here
	return false
}

// Errors returns any errors encountered during analysis
func (a *Analyzer) Errors() []string {
	return a.errors
}

// analyzeEnvironmentDivision analyzes the ENVIRONMENT DIVISION
func (a *Analyzer) analyzeEnvironmentDivision(div *ast.EnvironmentDivision) error {
	if div == nil {
		return nil // ENVIRONMENT DIVISION is optional
	}

	// Create new scope for ENVIRONMENT DIVISION
	prevScope := a.currentScope
	a.currentScope = newScope(prevScope)
	defer func() { a.currentScope = prevScope }()

	// Analyze CONFIGURATION SECTION
	if div.ConfigSection != nil {
		if err := a.analyzeConfigurationSection(div.ConfigSection); err != nil {
			return err
		}
	}

	// Analyze INPUT-OUTPUT SECTION
	if div.InputOutputSection != nil {
		if err := a.analyzeInputOutputSection(div.InputOutputSection); err != nil {
			return err
		}
	}

	return nil
}

// analyzeConfigurationSection analyzes the CONFIGURATION SECTION
func (a *Analyzer) analyzeConfigurationSection(section *ast.ConfigurationSection) error {
	if section == nil {
		return nil
	}
	// Configuration section validation can be added here
	return nil
}

// analyzeInputOutputSection analyzes the INPUT-OUTPUT SECTION
func (a *Analyzer) analyzeInputOutputSection(section *ast.InputOutputSection) error {
	if section == nil {
		return nil
	}

	// Analyze file control entries
	for _, entry := range section.FileControl {
		if err := a.analyzeFileControlEntry(entry); err != nil {
			return err
		}
	}

	return nil
}

// analyzeFileControlEntry analyzes a file control entry
func (a *Analyzer) analyzeFileControlEntry(entry *ast.FileControlEntry) error {
	if entry == nil {
		return nil
	}

	if entry.FileName == "" {
		return fmt.Errorf("missing file name in FILE-CONTROL entry")
	}

	// Add file to symbol table
	symbol := &Symbol{
		Name:       entry.FileName,
		Type:       TYPE_UNKNOWN, // File type will be determined by FD entry
		IsFileDesc: true,
	}
	a.currentScope.symbols[entry.FileName] = symbol

	return nil
}

// analyzeFileSection analyzes the FILE SECTION
func (a *Analyzer) analyzeFileSection(section *ast.FileSection) error {
	if section == nil {
		return nil
	}

	// Create new scope for FILE SECTION
	prevScope := a.currentScope
	a.currentScope = newScope(prevScope)
	a.currentScope.isFileDesc = true
	defer func() { a.currentScope = prevScope }()

	// Analyze each file description
	for _, fd := range section.Files {
		if err := a.analyzeFileDescription(fd); err != nil {
			return err
		}
	}

	return nil
}

// analyzeFileDescription analyzes a file description entry
func (a *Analyzer) analyzeFileDescription(fd *ast.FileDescription) error {
	if fd == nil {
		return nil
	}

	// Create new scope for this file
	prevScope := a.currentScope
	a.currentScope = newScope(prevScope)
	a.currentScope.isFileDesc = true
	defer func() { a.currentScope = prevScope }()

	// Analyze records
	for _, record := range fd.Records {
		if err := a.analyzeDataRecord(record); err != nil {
			return err
		}
	}

	return nil
}

// analyzeWorkingStorageSection analyzes the WORKING-STORAGE SECTION
func (a *Analyzer) analyzeWorkingStorageSection(section *ast.WorkingStorageSection) error {
	if section == nil {
		return nil
	}

	// Create new scope for WORKING-STORAGE SECTION
	prevScope := a.currentScope
	a.currentScope = newScope(prevScope)
	defer func() { a.currentScope = prevScope }()

	// Analyze each record
	for _, record := range section.Records {
		if err := a.analyzeDataRecord(record); err != nil {
			return err
		}
	}

	return nil
}

// analyzeComputeStatement analyzes a COMPUTE statement
func (a *Analyzer) analyzeComputeStatement(stmt *ast.ComputeStatement) error {
	if stmt == nil {
		return nil
	}

	// Check that target is numeric
	targetType := a.getExpressionType(stmt.Target)
	if targetType != TYPE_NUMERIC {
		return fmt.Errorf("COMPUTE target must be numeric")
	}

	// Check that value expression is numeric
	valueType := a.getExpressionType(stmt.Value)
	if valueType != TYPE_NUMERIC {
		return fmt.Errorf("COMPUTE value must be numeric")
	}

	return nil
}

// analyzeIfStatement analyzes an IF statement
func (a *Analyzer) analyzeIfStatement(stmt *ast.IfStatement) error {
	if stmt == nil {
		return nil
	}

	// Create new scope for IF block
	prevScope := a.currentScope
	a.currentScope = newScope(prevScope)
	defer func() { a.currentScope = prevScope }()

	// Analyze condition
	condType := a.getExpressionType(stmt.Condition)
	if condType == TYPE_UNKNOWN {
		return fmt.Errorf("invalid condition in IF statement")
	}

	// Analyze consequence statements
	for _, s := range stmt.Consequence {
		if err := a.analyzeStatement(s); err != nil {
			return err
		}
	}

	// Analyze alternative statements if present
	if len(stmt.Alternative) > 0 {
		altScope := newScope(prevScope)
		a.currentScope = altScope
		for _, s := range stmt.Alternative {
			if err := a.analyzeStatement(s); err != nil {
				return err
			}
		}
	}

	return nil
}

// analyzePerformStatement analyzes a PERFORM statement
func (a *Analyzer) analyzePerformStatement(stmt *ast.PerformStatement) error {
	if stmt == nil {
		return nil
	}

	// Create new scope for PERFORM block
	prevScope := a.currentScope
	a.currentScope = newScope(prevScope)
	defer func() { a.currentScope = prevScope }()

	// Check TIMES clause if present
	if stmt.Times != nil {
		timesType := a.getExpressionType(stmt.Times)
		if timesType != TYPE_NUMERIC {
			return fmt.Errorf("PERFORM TIMES value must be numeric")
		}
	}

	// Check UNTIL condition if present
	if stmt.Until != nil {
		untilType := a.getExpressionType(stmt.Until)
		if untilType == TYPE_UNKNOWN {
			return fmt.Errorf("invalid condition in PERFORM UNTIL")
		}
	}

	// Check VARYING phrase if present
	if stmt.Varying != nil {
		if err := a.analyzeVaryingPhrase(stmt.Varying); err != nil {
			return err
		}
	}

	// Analyze body statements
	for _, s := range stmt.Body {
		if err := a.analyzeStatement(s); err != nil {
			return err
		}
	}

	return nil
}

// analyzeVaryingPhrase analyzes the VARYING phrase of a PERFORM statement
func (a *Analyzer) analyzeVaryingPhrase(varying *ast.VaryingPhrase) error {
	if varying == nil {
		return nil
	}

	// Check that variable is numeric
	varType := a.getExpressionType(varying.Variable)
	if varType != TYPE_NUMERIC {
		return fmt.Errorf("VARYING variable must be numeric")
	}

	// Check FROM expression
	fromType := a.getExpressionType(varying.From)
	if fromType != TYPE_NUMERIC {
		return fmt.Errorf("FROM value must be numeric")
	}

	// Check BY expression
	byType := a.getExpressionType(varying.By)
	if byType != TYPE_NUMERIC {
		return fmt.Errorf("BY value must be numeric")
	}

	// Check UNTIL condition
	untilType := a.getExpressionType(varying.Until)
	if untilType == TYPE_UNKNOWN {
		return fmt.Errorf("invalid condition in VARYING UNTIL")
	}

	return nil
}

// analyzeDisplayStatement analyzes a DISPLAY statement
func (a *Analyzer) analyzeDisplayStatement(stmt *ast.DisplayStatement) error {
	if stmt == nil {
		return nil
	}

	// Check each value to be displayed
	for _, value := range stmt.Values {
		if a.getExpressionType(value) == TYPE_UNKNOWN {
			return fmt.Errorf("invalid value in DISPLAY statement")
		}
	}

	return nil
}
