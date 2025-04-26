package parser

import (
	"fmt"

	"github.com/mattwebdev/gobol/internal/ast"
	"github.com/mattwebdev/gobol/pkg/token"
)

// Parser represents a COBOL parser
type Parser struct {
	lexer        Lexer
	currentToken token.TokenInfo
	peekToken    token.TokenInfo
	errors       []string
}

// Lexer interface defines the methods needed from a lexer
type Lexer interface {
	NextToken() token.TokenInfo
}

// New creates a new Parser instance
func New(l Lexer) *Parser {
	p := &Parser{lexer: l}

	// Read two tokens to initialize currentToken and peekToken
	p.nextToken()
	p.nextToken()

	return p
}

// Errors returns parser errors
func (p *Parser) Errors() []string {
	return p.errors
}

// nextToken advances the parser to the next token
func (p *Parser) nextToken() {
	p.currentToken = p.peekToken
	p.peekToken = p.lexer.NextToken()
}

// ParseProgram parses a complete COBOL program
func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}

	// Parse each division in order
	program.IdentificationDivision = p.parseIdentificationDivision()
	program.EnvironmentDivision = p.parseEnvironmentDivision()
	program.DataDivision = p.parseDataDivision()
	program.ProcedureDivision = p.parseProcedureDivision()

	return program
}

// parseIdentificationDivision parses the IDENTIFICATION DIVISION
func (p *Parser) parseIdentificationDivision() *ast.IdentificationDivision {
	if !p.expectToken(token.IDENTIFICATION_DIVISION) {
		return nil
	}

	division := &ast.IdentificationDivision{
		Token: p.currentToken,
	}

	// Parse PROGRAM-ID
	if p.currentToken.Type == token.PROGRAM_ID {
		p.nextToken() // Move past PROGRAM-ID
		if p.currentToken.Type != token.DOT {
			division.ProgramID = &ast.ProgramID{
				Token: p.currentToken,
				Name:  p.currentToken.Literal,
			}
		}
		p.expectToken(token.DOT)
	}

	// Parse optional entries (AUTHOR, DATE-WRITTEN, etc.)
	for p.currentToken.Type != token.EOF &&
		p.currentToken.Type != token.ENVIRONMENT_DIVISION &&
		p.currentToken.Type != token.DATA_DIVISION &&
		p.currentToken.Type != token.PROCEDURE_DIVISION {

		switch p.currentToken.Type {
		case token.AUTHOR:
			p.nextToken()
			division.Author = p.currentToken.Literal
			p.expectToken(token.DOT)
		case token.DATE_WRITTEN:
			p.nextToken()
			division.Written = p.currentToken.Literal
			p.expectToken(token.DOT)
		default:
			p.nextToken()
		}
	}

	return division
}

// parseEnvironmentDivision parses the ENVIRONMENT DIVISION
func (p *Parser) parseEnvironmentDivision() *ast.EnvironmentDivision {
	if !p.expectToken(token.ENVIRONMENT_DIVISION) {
		return nil
	}

	division := &ast.EnvironmentDivision{
		Token: p.currentToken,
	}

	// Parse CONFIGURATION SECTION
	if p.currentToken.Type == token.CONFIGURATION && p.peekTokenIs(token.SECTION) {
		division.ConfigSection = p.parseConfigurationSection()
	}

	// Parse INPUT-OUTPUT SECTION
	if p.currentToken.Type == token.FILE && p.peekTokenIs(token.SECTION) {
		division.InputOutputSection = p.parseInputOutputSection()
	}

	return division
}

// parseDataDivision parses the DATA DIVISION
func (p *Parser) parseDataDivision() *ast.DataDivision {
	if !p.expectToken(token.DATA_DIVISION) {
		return nil
	}

	division := &ast.DataDivision{
		Token: p.currentToken,
	}

	// Parse sections until we hit another division or EOF
	for p.currentToken.Type != token.EOF &&
		p.currentToken.Type != token.PROCEDURE_DIVISION {

		switch {
		case p.currentToken.Type == token.FILE && p.peekTokenIs(token.SECTION):
			division.FileSection = p.parseFileSection()

		case p.currentToken.Type == token.WORKING_STORAGE && p.peekTokenIs(token.SECTION):
			division.WorkingStorage = p.parseWorkingStorageSection()

		case p.currentToken.Type == token.LOCAL_STORAGE && p.peekTokenIs(token.SECTION):
			division.LocalStorage = p.parseLocalStorageSection()

		case p.currentToken.Type == token.LINKAGE && p.peekTokenIs(token.SECTION):
			division.LinkageSection = p.parseLinkageSection()

		default:
			p.nextToken()
		}
	}

	return division
}

// parseProcedureDivision parses the PROCEDURE DIVISION
func (p *Parser) parseProcedureDivision() *ast.ProcedureDivision {
	if !p.expectToken(token.PROCEDURE_DIVISION) {
		return nil
	}

	division := &ast.ProcedureDivision{
		Token: p.currentToken,
	}

	// Parse statements until EOF or another division
	for p.currentToken.Type != token.EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			division.Statements = append(division.Statements, stmt)
		}
	}

	return division
}

// parseStatement parses a single statement
func (p *Parser) parseStatement() ast.Statement {
	switch p.currentToken.Type {
	case token.MOVE:
		return p.parseMoveStatement()
	case token.COMPUTE:
		return p.parseComputeStatement()
	case token.IF:
		return p.parseIfStatement()
	case token.PERFORM:
		return p.parsePerformStatement()
	case token.DISPLAY:
		return p.parseDisplayStatement()
	case token.TRY:
		return p.parseTryStatement()
	case token.RAISE:
		return p.parseRaiseStatement()
	default:
		p.nextToken()
		return nil
	}
}

// expectToken checks if the current token is of the expected type
// and advances to the next token if it is
func (p *Parser) expectToken(t token.Token) bool {
	if p.currentToken.Type == t {
		p.nextToken()
		return true
	}
	p.addError(fmt.Sprintf("expected token %s, got %s instead", t, p.currentToken.Type))
	return false
}

// addError adds an error to the parser's error list
func (p *Parser) addError(msg string) {
	p.errors = append(p.errors, msg)
}

// curTokenIs returns true if the current token is of the given type
func (p *Parser) curTokenIs(t token.Token) bool {
	return p.currentToken.Type == t
}

// peekTokenIs returns true if the peek token is of the given type
func (p *Parser) peekTokenIs(t token.Token) bool {
	return p.peekToken.Type == t
}

// parseConfigurationSection parses the CONFIGURATION SECTION
func (p *Parser) parseConfigurationSection() *ast.ConfigurationSection {
	p.nextToken() // Move past CONFIGURATION
	p.expectToken(token.SECTION)

	section := &ast.ConfigurationSection{
		Token: p.currentToken,
	}

	// Parse SOURCE-COMPUTER and OBJECT-COMPUTER paragraphs
	for p.currentToken.Type != token.EOF &&
		p.currentToken.Type != token.FILE &&
		p.currentToken.Type != token.DATA_DIVISION {
		switch p.currentToken.Type {
		case token.SOURCE_COMPUTER:
			p.nextToken()
			section.SourceComputer = p.currentToken.Literal
			p.expectToken(token.DOT)
		case token.OBJECT_COMPUTER:
			p.nextToken()
			section.ObjectComputer = p.currentToken.Literal
			p.expectToken(token.DOT)
		default:
			p.nextToken()
		}
	}

	return section
}

// parseInputOutputSection parses the INPUT-OUTPUT SECTION
func (p *Parser) parseInputOutputSection() *ast.InputOutputSection {
	p.nextToken() // Move past FILE
	p.expectToken(token.SECTION)

	section := &ast.InputOutputSection{
		Token: p.currentToken,
	}

	// Parse FILE-CONTROL entries
	for p.currentToken.Type != token.EOF &&
		p.currentToken.Type != token.DATA_DIVISION {
		entry := p.parseFileControlEntry()
		if entry != nil {
			section.FileControl = append(section.FileControl, entry)
		}
	}

	return section
}

// parseFileControlEntry parses a single file control entry
func (p *Parser) parseFileControlEntry() *ast.FileControlEntry {
	if p.currentToken.Type != token.FILE {
		return nil
	}

	entry := &ast.FileControlEntry{
		Token: p.currentToken,
	}

	// Parse file name
	if p.peekTokenIs(token.IDENT) {
		p.nextToken()
		entry.FileName = p.currentToken.Literal
	}

	// Parse ASSIGN TO
	if p.peekTokenIs(token.ASSIGN) {
		p.nextToken()
		p.expectToken(token.TO)
		entry.Assign = p.currentToken.Literal
	}

	// Parse ORGANIZATION
	if p.peekTokenIs(token.ORGANIZATION) {
		p.nextToken()
		entry.Organization = p.currentToken.Literal
	}

	// Parse ACCESS MODE
	if p.peekTokenIs(token.ACCESS) {
		p.nextToken()
		entry.AccessMode = p.currentToken.Literal
	}

	p.expectToken(token.DOT)
	return entry
}

// parseFileSection parses the FILE SECTION
func (p *Parser) parseFileSection() *ast.FileSection {
	p.nextToken() // Move past FILE
	p.expectToken(token.SECTION)

	section := &ast.FileSection{
		Token: p.currentToken,
	}

	// Parse file descriptions (FD entries)
	for p.currentToken.Type != token.EOF &&
		p.currentToken.Type != token.WORKING_STORAGE {
		fd := p.parseFileDescription()
		if fd != nil {
			section.Files = append(section.Files, fd)
		}
	}

	return section
}

// parseWorkingStorageSection parses the WORKING-STORAGE SECTION
func (p *Parser) parseWorkingStorageSection() *ast.WorkingStorageSection {
	p.nextToken() // Move past WORKING-STORAGE
	p.expectToken(token.SECTION)

	section := &ast.WorkingStorageSection{
		Token: p.currentToken,
	}

	// Parse data records
	for p.currentToken.Type != token.EOF &&
		p.currentToken.Type != token.PROCEDURE_DIVISION {
		record := p.parseDataRecord()
		if record != nil {
			section.Records = append(section.Records, record)
		}
	}

	return section
}

// parseDataRecord parses a data record entry
func (p *Parser) parseDataRecord() *ast.DataRecord {
	if !token.IsLevelNumber(p.currentToken.Literal) {
		return nil
	}

	record := &ast.DataRecord{
		LevelNumber: p.currentToken.Literal,
	}

	// Parse name
	if p.peekTokenIs(token.IDENT) {
		p.nextToken()
		record.Name = p.currentToken.Literal
	}

	// Parse PIC clause
	if p.peekTokenIs(token.PIC) {
		p.nextToken()
		record.Picture = p.parsePictureClause()
	}

	// Parse VALUE clause
	if p.peekTokenIs(token.VALUE) {
		p.nextToken()
		record.Value = p.parseExpression()
	}

	return record
}

// parseMoveStatement parses a MOVE statement
func (p *Parser) parseMoveStatement() *ast.MoveStatement {
	stmt := &ast.MoveStatement{
		Token: p.currentToken,
	}

	// Parse source
	p.nextToken()
	stmt.Value = p.parseExpression()

	// Parse TO
	if !p.expectToken(token.TO) {
		return nil
	}

	// Parse targets
	for p.currentToken.Type != token.DOT {
		target := p.parseExpression()
		if target != nil {
			stmt.To = append(stmt.To, target)
		}
		if p.peekTokenIs(token.DOT) {
			break
		}
		p.nextToken()
	}

	p.expectToken(token.DOT)
	return stmt
}

// parseComputeStatement parses a COMPUTE statement
func (p *Parser) parseComputeStatement() *ast.ComputeStatement {
	stmt := &ast.ComputeStatement{
		Token: p.currentToken,
	}

	// Parse target
	p.nextToken()
	stmt.Target = p.parseExpression()

	// Parse EQUAL
	if !p.expectToken(token.EQUAL) {
		return nil
	}

	// Parse expression
	stmt.Value = p.parseExpression()

	p.expectToken(token.DOT)
	return stmt
}

// parseIfStatement parses an IF statement
func (p *Parser) parseIfStatement() *ast.IfStatement {
	stmt := &ast.IfStatement{
		Token: p.currentToken,
	}

	// Parse condition
	p.nextToken()
	stmt.Condition = p.parseExpression()

	// Parse consequence
	for p.currentToken.Type != token.ELSE &&
		p.currentToken.Type != token.END_IF &&
		p.currentToken.Type != token.EOF {
		s := p.parseStatement()
		if s != nil {
			stmt.Consequence = append(stmt.Consequence, s)
		}
	}

	// Parse ELSE clause if present
	if p.currentToken.Type == token.ELSE {
		p.nextToken()
		for p.currentToken.Type != token.END_IF &&
			p.currentToken.Type != token.EOF {
			s := p.parseStatement()
			if s != nil {
				stmt.Alternative = append(stmt.Alternative, s)
			}
		}
	}

	p.expectToken(token.END_IF)
	p.expectToken(token.DOT)
	return stmt
}

// parsePerformStatement parses a PERFORM statement
func (p *Parser) parsePerformStatement() *ast.PerformStatement {
	stmt := &ast.PerformStatement{
		Token: p.currentToken,
	}

	p.nextToken()

	// Parse TIMES if present
	if p.peekTokenIs(token.TIMES) {
		stmt.Times = p.parseExpression()
		p.nextToken()
	}

	// Parse UNTIL if present
	if p.currentToken.Type == token.UNTIL {
		p.nextToken()
		stmt.Until = p.parseExpression()
	}

	// Parse VARYING if present
	if p.currentToken.Type == token.VARYING {
		stmt.Varying = p.parseVaryingPhrase()
	}

	// Parse body
	for p.currentToken.Type != token.END_PERFORM &&
		p.currentToken.Type != token.EOF {
		s := p.parseStatement()
		if s != nil {
			stmt.Body = append(stmt.Body, s)
		}
	}

	p.expectToken(token.END_PERFORM)
	p.expectToken(token.DOT)
	return stmt
}

// parseVaryingPhrase parses the VARYING phrase of a PERFORM statement
func (p *Parser) parseVaryingPhrase() *ast.VaryingPhrase {
	p.nextToken() // Move past VARYING

	phrase := &ast.VaryingPhrase{
		Variable: p.parseExpression(),
	}

	if !p.expectToken(token.FROM) {
		return nil
	}
	phrase.From = p.parseExpression()

	if !p.expectToken(token.BY_KEYWORD) {
		return nil
	}
	phrase.By = p.parseExpression()

	if !p.expectToken(token.UNTIL) {
		return nil
	}
	phrase.Until = p.parseExpression()

	return phrase
}

// parseDisplayStatement parses a DISPLAY statement
func (p *Parser) parseDisplayStatement() *ast.DisplayStatement {
	stmt := &ast.DisplayStatement{
		Token: p.currentToken,
	}

	// Parse values to display
	for p.currentToken.Type != token.DOT &&
		p.currentToken.Type != token.EOF {
		p.nextToken()
		value := p.parseExpression()
		if value != nil {
			stmt.Values = append(stmt.Values, value)
		}
	}

	p.expectToken(token.DOT)
	return stmt
}

// parseExpression parses an expression
func (p *Parser) parseExpression() ast.Expression {
	switch p.currentToken.Type {
	case token.IDENT:
		return &ast.Identifier{
			Token: p.currentToken,
			Value: p.currentToken.Literal,
		}
	case token.STRING_LIT:
		return &ast.StringLiteral{
			Token: p.currentToken,
			Value: p.currentToken.Literal,
		}
	case token.NUMERIC:
		return &ast.NumericLiteral{
			Token: p.currentToken,
			Value: p.currentToken.Literal,
		}
	case token.ZERO, token.ZEROS, token.ZEROES,
		token.SPACE, token.SPACES,
		token.HIGH_VALUE, token.HIGH_VALUES,
		token.LOW_VALUE, token.LOW_VALUES:
		return &ast.FigurativeConstant{
			Token: p.currentToken,
			Type:  p.currentToken.Type,
		}
	default:
		p.addError(fmt.Sprintf("unexpected token %s in expression", p.currentToken.Type))
		return nil
	}
}

// parseFileDescription parses a file description (FD) entry
func (p *Parser) parseFileDescription() *ast.FileDescription {
	if p.currentToken.Type != token.FILE {
		return nil
	}

	fd := &ast.FileDescription{
		Token: p.currentToken,
	}

	// Parse file name
	if p.peekTokenIs(token.IDENT) {
		p.nextToken()
		fd.Name = p.currentToken.Literal
	}

	// Parse records
	for p.currentToken.Type != token.EOF &&
		!p.peekTokenIs(token.FILE) {
		record := p.parseDataRecord()
		if record != nil {
			fd.Records = append(fd.Records, record)
		}
		p.nextToken()
	}

	return fd
}

// parsePictureClause parses a PIC clause
func (p *Parser) parsePictureClause() *ast.PictureClause {
	if p.currentToken.Type != token.PIC {
		return nil
	}

	pic := &ast.PictureClause{
		Token: p.currentToken,
	}

	// Move to the picture string
	p.nextToken()

	// Parse the picture string
	picString := p.currentToken.Literal
	pic.Type = string(picString[0]) // X, 9, etc.

	// Count length and check for decimal point
	length := 0
	decimalPos := 0
	isEdited := false
	editMask := ""

	for _, ch := range picString {
		switch ch {
		case 'X', '9':
			length++
		case 'V', '.':
			decimalPos = length
		case '$', '+', '-', ',', 'Z', '*', 'B':
			isEdited = true
			editMask += string(ch)
		}
	}

	pic.Length = length
	pic.DecimalPos = decimalPos
	pic.IsEdited = isEdited
	pic.EditMask = editMask

	return pic
}

// parseLocalStorageSection parses the LOCAL-STORAGE SECTION
func (p *Parser) parseLocalStorageSection() *ast.LocalStorageSection {
	section := &ast.LocalStorageSection{
		Token: p.currentToken,
	}

	p.nextToken() // Move past LOCAL-STORAGE
	p.expectToken(token.SECTION)
	p.expectToken(token.DOT)

	// Parse data records until we hit another section or division
	for p.currentToken.Type != token.EOF &&
		!p.isStartOfSection() &&
		!p.isStartOfDivision() {

		if record := p.parseDataRecord(); record != nil {
			section.Records = append(section.Records, record)
		}
	}

	return section
}

// isStartOfSection returns true if current tokens indicate start of a new section
func (p *Parser) isStartOfSection() bool {
	return (p.currentToken.Type == token.FILE && p.peekTokenIs(token.SECTION)) ||
		(p.currentToken.Type == token.WORKING_STORAGE && p.peekTokenIs(token.SECTION)) ||
		(p.currentToken.Type == token.LOCAL_STORAGE && p.peekTokenIs(token.SECTION)) ||
		(p.currentToken.Type == token.LINKAGE && p.peekTokenIs(token.SECTION))
}

// isStartOfDivision returns true if current token indicates start of a new division
func (p *Parser) isStartOfDivision() bool {
	return p.currentToken.Type == token.PROCEDURE_DIVISION ||
		p.currentToken.Type == token.ENVIRONMENT_DIVISION ||
		p.currentToken.Type == token.IDENTIFICATION_DIVISION
}

// parseLinkageSection parses the LINKAGE SECTION
func (p *Parser) parseLinkageSection() *ast.LinkageSection {
	section := &ast.LinkageSection{
		Token: p.currentToken,
	}

	p.nextToken() // Move past LINKAGE
	p.expectToken(token.SECTION)
	p.expectToken(token.DOT)

	// Parse data records until we hit another section or division
	for p.currentToken.Type != token.EOF &&
		!p.isStartOfSection() &&
		!p.isStartOfDivision() {

		if record := p.parseDataRecord(); record != nil {
			section.Records = append(section.Records, record)
		}
	}

	return section
}

// parseTryStatement parses a TRY-CATCH-FINALLY block
func (p *Parser) parseTryStatement() *ast.TryStatement {
	stmt := &ast.TryStatement{
		Token: p.currentToken,
	}

	// Parse TRY block
	p.nextToken() // Move past TRY

	// Parse statements in TRY block until CATCH, FINALLY, or END-TRY
	for p.currentToken.Type != token.EOF &&
		p.currentToken.Type != token.CATCH &&
		p.currentToken.Type != token.FINALLY &&
		p.currentToken.Type != token.END_TRY {
		if s := p.parseStatement(); s != nil {
			stmt.TryBlock = append(stmt.TryBlock, s)
		}
	}

	// Parse CATCH blocks
	for p.currentToken.Type == token.CATCH {
		catchBlock := p.parseCatchBlock()
		if catchBlock != nil {
			stmt.CatchBlocks = append(stmt.CatchBlocks, catchBlock)
		}
	}

	// Parse optional FINALLY block
	if p.currentToken.Type == token.FINALLY {
		p.nextToken() // Move past FINALLY

		for p.currentToken.Type != token.EOF &&
			p.currentToken.Type != token.END_TRY {
			if s := p.parseStatement(); s != nil {
				stmt.FinallyBlock = append(stmt.FinallyBlock, s)
			}
		}
	}

	// Expect END-TRY
	if !p.expectToken(token.END_TRY) {
		return nil
	}

	p.expectToken(token.DOT)
	return stmt
}

// parseCatchBlock parses a single CATCH block
func (p *Parser) parseCatchBlock() *ast.CatchBlock {
	block := &ast.CatchBlock{
		Token: p.currentToken,
	}

	p.nextToken() // Move past CATCH

	// Parse optional exception type
	if p.isExceptionType(p.currentToken.Type) {
		block.ExceptType = p.currentToken.Type
		p.nextToken()

		// Parse optional AS clause
		if p.currentToken.Literal == "AS" {
			p.nextToken()
			if p.currentToken.Type == token.IDENT {
				block.Name = p.currentToken.Literal
				p.nextToken()
			}
		}
	}

	// Parse CATCH block statements
	for p.currentToken.Type != token.EOF &&
		p.currentToken.Type != token.CATCH &&
		p.currentToken.Type != token.FINALLY &&
		p.currentToken.Type != token.END_TRY {
		if s := p.parseStatement(); s != nil {
			block.Body = append(block.Body, s)
		}
	}

	return block
}

// parseRaiseStatement parses a RAISE statement
func (p *Parser) parseRaiseStatement() *ast.RaiseStatement {
	stmt := &ast.RaiseStatement{
		Token: p.currentToken,
	}

	p.nextToken() // Move past RAISE

	// Parse optional expression to raise
	if p.currentToken.Type != token.DOT {
		stmt.Value = p.parseExpression()
	}

	p.expectToken(token.DOT)
	return stmt
}

// isExceptionType returns true if the token represents a valid exception type
func (p *Parser) isExceptionType(t token.Token) bool {
	switch t {
	case token.SIZE_ERROR:
		return true
	// Add other exception types here as needed
	default:
		return false
	}
}
