package ast

import (
	"fmt"

	"github.com/mattwebdev/gobol/pkg/token"
)

// Node represents a node in the AST
type Node interface {
	TokenLiteral() string // Used for debugging/testing
	String() string       // Used for debugging/testing
}

// Statement represents a statement node in the AST
type Statement interface {
	Node
	statementNode()
}

// Expression represents an expression node in the AST
type Expression interface {
	Node
	expressionNode()
}

// Program represents a COBOL program
type Program struct {
	IdentificationDivision *IdentificationDivision
	EnvironmentDivision    *EnvironmentDivision
	DataDivision           *DataDivision
	ProcedureDivision      *ProcedureDivision
}

func (p *Program) TokenLiteral() string {
	if p.IdentificationDivision != nil {
		return p.IdentificationDivision.TokenLiteral()
	}
	return ""
}

func (p *Program) String() string {
	var out string
	if p.IdentificationDivision != nil {
		out += p.IdentificationDivision.String()
	}
	if p.EnvironmentDivision != nil {
		out += p.EnvironmentDivision.String()
	}
	if p.DataDivision != nil {
		out += p.DataDivision.String()
	}
	if p.ProcedureDivision != nil {
		out += p.ProcedureDivision.String()
	}
	return out
}

// IdentificationDivision represents the IDENTIFICATION DIVISION
type IdentificationDivision struct {
	Token     token.TokenInfo
	ProgramID *ProgramID
	Author    string
	Written   string
}

func (id *IdentificationDivision) statementNode()       {}
func (id *IdentificationDivision) TokenLiteral() string { return id.Token.Literal }
func (id *IdentificationDivision) String() string {
	return "IDENTIFICATION DIVISION.\n" + id.ProgramID.String()
}

// ProgramID represents the PROGRAM-ID statement
type ProgramID struct {
	Token token.TokenInfo
	Name  string
}

func (p *ProgramID) statementNode()       {}
func (p *ProgramID) TokenLiteral() string { return p.Token.Literal }
func (p *ProgramID) String() string       { return "PROGRAM-ID. " + p.Name + ".\n" }

// EnvironmentDivision represents the ENVIRONMENT DIVISION
type EnvironmentDivision struct {
	Token              token.TokenInfo
	ConfigSection      *ConfigurationSection
	InputOutputSection *InputOutputSection
}

func (ed *EnvironmentDivision) statementNode()       {}
func (ed *EnvironmentDivision) TokenLiteral() string { return ed.Token.Literal }
func (ed *EnvironmentDivision) String() string {
	return "ENVIRONMENT DIVISION.\n"
}

// ConfigurationSection represents the CONFIGURATION SECTION
type ConfigurationSection struct {
	Token          token.TokenInfo
	SourceComputer string
	ObjectComputer string
	SpecialNames   []*SpecialName
}

// DataDivision represents the DATA DIVISION
type DataDivision struct {
	Token                token.TokenInfo
	FileSection          *FileSection
	WorkingStorage       *WorkingStorageSection
	LocalStorage         *LocalStorageSection
	LinkageSection       *LinkageSection
	CommunicationSection *CommunicationSection
}

func (dd *DataDivision) statementNode()       {}
func (dd *DataDivision) TokenLiteral() string { return dd.Token.Literal }
func (dd *DataDivision) String() string {
	return "DATA DIVISION.\n"
}

// WorkingStorageSection represents the WORKING-STORAGE SECTION
type WorkingStorageSection struct {
	Token   token.TokenInfo
	Records []*DataRecord
}

// LocalStorageSection represents the LOCAL-STORAGE SECTION
type LocalStorageSection struct {
	Token   token.TokenInfo
	Records []*DataRecord
}

func (ls *LocalStorageSection) statementNode()       {}
func (ls *LocalStorageSection) TokenLiteral() string { return ls.Token.Literal }
func (ls *LocalStorageSection) String() string {
	var out string
	out += "LOCAL-STORAGE SECTION.\n"
	for _, record := range ls.Records {
		out += "    " + record.LevelNumber + " " + record.Name
		if record.Picture != nil {
			out += " PIC " + record.Picture.Type
			if record.Picture.Length > 0 {
				out += "(" + fmt.Sprintf("%d", record.Picture.Length) + ")"
			}
		}
		if record.Value != nil {
			out += " VALUE " + record.Value.String()
		}
		out += ".\n"
	}
	return out
}

// DataRecord represents a data record (level number + description)
type DataRecord struct {
	LevelNumber string
	Name        string
	Picture     *PictureClause
	Value       Expression
	Occurs      *OccursClause
	Redefines   string
	IsCondition bool         // For 88 level items
	Values      []Expression // For 88 level items
}

// PictureClause represents a PIC clause
type PictureClause struct {
	Token      token.TokenInfo
	Type       string // X, 9, etc.
	Length     int
	DecimalPos int // For numeric items
	IsEdited   bool
	EditMask   string // For edited items
}

// OccursClause represents an OCCURS clause
type OccursClause struct {
	Token       token.TokenInfo
	MinTimes    int
	MaxTimes    int
	DependingOn string
	Keys        []*OccursKey
	Indexed     []string
}

// OccursKey represents a key in an OCCURS clause
type OccursKey struct {
	IsAscending bool
	KeyNames    []string
}

// ProcedureDivision represents the PROCEDURE DIVISION
type ProcedureDivision struct {
	Token      token.TokenInfo
	Using      []string
	Statements []Statement
}

func (pd *ProcedureDivision) statementNode()       {}
func (pd *ProcedureDivision) TokenLiteral() string { return pd.Token.Literal }
func (pd *ProcedureDivision) String() string {
	return "PROCEDURE DIVISION.\n"
}

// Statement Types

// MoveStatement represents a MOVE statement
type MoveStatement struct {
	Token token.TokenInfo
	Value Expression
	To    []Expression
}

func (ms *MoveStatement) statementNode()       {}
func (ms *MoveStatement) TokenLiteral() string { return ms.Token.Literal }

// ComputeStatement represents a COMPUTE statement
type ComputeStatement struct {
	Token  token.TokenInfo
	Target Expression
	Value  Expression
}

func (cs *ComputeStatement) statementNode()       {}
func (cs *ComputeStatement) TokenLiteral() string { return cs.Token.Literal }

// IfStatement represents an IF statement
type IfStatement struct {
	Token       token.TokenInfo
	Condition   Expression
	Consequence []Statement
	Alternative []Statement
}

func (is *IfStatement) statementNode()       {}
func (is *IfStatement) TokenLiteral() string { return is.Token.Literal }

// PerformStatement represents a PERFORM statement
type PerformStatement struct {
	Token   token.TokenInfo
	Times   Expression // For PERFORM n TIMES
	Until   Expression // For PERFORM UNTIL
	Varying *VaryingPhrase
	Body    []Statement
}

// VaryingPhrase represents the VARYING phrase in a PERFORM statement
type VaryingPhrase struct {
	Variable Expression
	From     Expression
	By       Expression
	Until    Expression
}

func (ps *PerformStatement) statementNode()       {}
func (ps *PerformStatement) TokenLiteral() string { return ps.Token.Literal }

// DisplayStatement represents a DISPLAY statement
type DisplayStatement struct {
	Token  token.TokenInfo
	Values []Expression
}

func (ds *DisplayStatement) statementNode()       {}
func (ds *DisplayStatement) TokenLiteral() string { return ds.Token.Literal }

// Expression Types

// Identifier represents a variable name
type Identifier struct {
	Token token.TokenInfo
	Value string
}

func (i *Identifier) expressionNode()      {}
func (i *Identifier) TokenLiteral() string { return i.Token.Literal }
func (i *Identifier) String() string       { return i.Value }

// StringLiteral represents a string literal
type StringLiteral struct {
	Token token.TokenInfo
	Value string
}

func (sl *StringLiteral) expressionNode()      {}
func (sl *StringLiteral) TokenLiteral() string { return sl.Token.Literal }
func (sl *StringLiteral) String() string       { return sl.Token.Literal }

// NumericLiteral represents a numeric literal
type NumericLiteral struct {
	Token token.TokenInfo
	Value string
}

func (nl *NumericLiteral) expressionNode()      {}
func (nl *NumericLiteral) TokenLiteral() string { return nl.Token.Literal }
func (nl *NumericLiteral) String() string       { return nl.Token.Literal }

// BinaryExpression represents a binary operation
type BinaryExpression struct {
	Token    token.TokenInfo
	Left     Expression
	Operator string
	Right    Expression
}

func (be *BinaryExpression) expressionNode()      {}
func (be *BinaryExpression) TokenLiteral() string { return be.Token.Literal }

// RelationalExpression represents a relational operation
type RelationalExpression struct {
	Token    token.TokenInfo
	Left     Expression
	Operator string
	Right    Expression
}

func (re *RelationalExpression) expressionNode()      {}
func (re *RelationalExpression) TokenLiteral() string { return re.Token.Literal }

// FigurativeConstant represents COBOL figurative constants
type FigurativeConstant struct {
	Token token.TokenInfo
	Type  token.Token // ZERO, SPACE, HIGH-VALUE, etc.
}

func (fc *FigurativeConstant) expressionNode()      {}
func (fc *FigurativeConstant) TokenLiteral() string { return fc.Token.Literal }
func (fc *FigurativeConstant) String() string       { return fc.Token.Literal }

// InputOutputSection represents the INPUT-OUTPUT SECTION
type InputOutputSection struct {
	Token       token.TokenInfo
	FileControl []*FileControlEntry
}

func (ios *InputOutputSection) statementNode()       {}
func (ios *InputOutputSection) TokenLiteral() string { return ios.Token.Literal }
func (ios *InputOutputSection) String() string {
	return "INPUT-OUTPUT SECTION.\n"
}

// FileControlEntry represents a FILE-CONTROL entry
type FileControlEntry struct {
	Token        token.TokenInfo
	FileName     string
	Assign       string
	Organization string
	AccessMode   string
}

func (fce *FileControlEntry) statementNode()       {}
func (fce *FileControlEntry) TokenLiteral() string { return fce.Token.Literal }
func (fce *FileControlEntry) String() string {
	return "SELECT " + fce.FileName + "\n"
}

// SpecialName represents a SPECIAL-NAMES entry
type SpecialName struct {
	Token   token.TokenInfo
	Name    string
	IsEqual string
}

func (sn *SpecialName) statementNode()       {}
func (sn *SpecialName) TokenLiteral() string { return sn.Token.Literal }
func (sn *SpecialName) String() string {
	return sn.Name + " IS " + sn.IsEqual + "\n"
}

// FileSection represents the FILE SECTION
type FileSection struct {
	Token token.TokenInfo
	Files []*FileDescription
}

func (fs *FileSection) statementNode()       {}
func (fs *FileSection) TokenLiteral() string { return fs.Token.Literal }
func (fs *FileSection) String() string {
	return "FILE SECTION.\n"
}

// FileDescription represents an FD entry
type FileDescription struct {
	Token   token.TokenInfo
	Name    string
	Records []*DataRecord
}

func (fd *FileDescription) statementNode()       {}
func (fd *FileDescription) TokenLiteral() string { return fd.Token.Literal }
func (fd *FileDescription) String() string {
	return "FD " + fd.Name + ".\n"
}

// LinkageSection represents the LINKAGE SECTION
type LinkageSection struct {
	Token   token.TokenInfo
	Records []*DataRecord
}

func (ls *LinkageSection) statementNode()       {}
func (ls *LinkageSection) TokenLiteral() string { return ls.Token.Literal }
func (ls *LinkageSection) String() string {
	return "LINKAGE SECTION.\n"
}

// CommunicationSection represents the COMMUNICATION SECTION
type CommunicationSection struct {
	Token   token.TokenInfo
	Records []*DataRecord
}

func (cs *CommunicationSection) statementNode()       {}
func (cs *CommunicationSection) TokenLiteral() string { return cs.Token.Literal }
func (cs *CommunicationSection) String() string {
	return "COMMUNICATION SECTION.\n"
}

// String methods for statement types
func (ms *MoveStatement) String() string {
	var out string
	out += "MOVE " + ms.Value.String() + " TO"
	for _, target := range ms.To {
		out += " " + target.String()
	}
	out += "."
	return out
}

func (cs *ComputeStatement) String() string {
	return "COMPUTE " + cs.Target.String() + " = " + cs.Value.String() + "."
}

func (is *IfStatement) String() string {
	var out string
	out += "IF " + is.Condition.String() + " "
	for _, stmt := range is.Consequence {
		out += stmt.String() + " "
	}
	if len(is.Alternative) > 0 {
		out += "ELSE "
		for _, stmt := range is.Alternative {
			out += stmt.String() + " "
		}
	}
	out += "END-IF."
	return out
}

func (ps *PerformStatement) String() string {
	var out string
	out += "PERFORM"
	if ps.Times != nil {
		out += " " + ps.Times.String() + " TIMES"
	}
	if ps.Until != nil {
		out += " UNTIL " + ps.Until.String()
	}
	if ps.Varying != nil {
		out += " VARYING " + ps.Varying.String()
	}
	for _, stmt := range ps.Body {
		out += " " + stmt.String()
	}
	out += " END-PERFORM."
	return out
}

func (ds *DisplayStatement) String() string {
	var out string
	out += "DISPLAY"
	for _, value := range ds.Values {
		out += " " + value.String()
	}
	out += "."
	return out
}

func (vp *VaryingPhrase) String() string {
	return vp.Variable.String() +
		" FROM " + vp.From.String() +
		" BY " + vp.By.String() +
		" UNTIL " + vp.Until.String()
}
