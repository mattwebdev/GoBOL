package token

// DataTypeInfo contains information about a COBOL data type
type DataTypeInfo struct {
	Token    Token   // The token representing this data type
	Class    string  // The COBOL class (numeric, alphanumeric, etc.)
	Category string  // The COBOL category (binary, float, etc.)
	Aliases  []Token // Alternative tokens that represent the same type
	Usage    string  // The USAGE clause value for this type
}

// dataTypeInfos maps tokens to their data type information
var dataTypeInfos = map[Token]DataTypeInfo{
	BINARY_CHAR: {
		Token:    BINARY_CHAR,
		Class:    "numeric",
		Category: "binary",
		Usage:    "BINARY-CHAR",
	},
	BINARY_SHORT: {
		Token:    BINARY_SHORT,
		Class:    "numeric",
		Category: "binary",
		Usage:    "BINARY-SHORT",
	},
	BINARY_LONG: {
		Token:    BINARY_LONG,
		Class:    "numeric",
		Category: "binary",
		Usage:    "BINARY-LONG",
	},
	BINARY_DOUBLE: {
		Token:    BINARY_DOUBLE,
		Class:    "numeric",
		Category: "binary",
		Usage:    "BINARY-DOUBLE",
	},
	FLOAT_SHORT: {
		Token:    FLOAT_SHORT,
		Class:    "numeric",
		Category: "float",
		Usage:    "FLOAT-SHORT",
	},
	FLOAT_LONG: {
		Token:    FLOAT_LONG,
		Class:    "numeric",
		Category: "float",
		Usage:    "FLOAT-LONG",
	},
	FLOAT_EXTENDED: {
		Token:    FLOAT_EXTENDED,
		Class:    "numeric",
		Category: "float",
		Usage:    "FLOAT-EXTENDED",
	},
	POINTER_32: {
		Token:    POINTER_32,
		Class:    "pointer",
		Category: "pointer",
		Usage:    "POINTER-32",
	},
	PROCEDURE_POINTER: {
		Token:    PROCEDURE_POINTER,
		Class:    "pointer",
		Category: "procedure-pointer",
		Usage:    "PROCEDURE-POINTER",
	},
	OBJECT: {
		Token:    OBJECT,
		Class:    "object",
		Category: "object",
		Usage:    "OBJECT",
	},
}

// IsDataType returns true if the token represents a data type
func IsDataType(tok Token) bool {
	_, ok := dataTypeInfos[tok]
	return ok
}

// GetDataTypeInfo returns information about a data type token
func GetDataTypeInfo(tok Token) (DataTypeInfo, bool) {
	info, ok := dataTypeInfos[tok]
	return info, ok
}

// GetDataTypeCategory returns the category of a data type token
func GetDataTypeCategory(tok Token) string {
	if info, ok := dataTypeInfos[tok]; ok {
		return info.Category
	}
	return ""
}

// GetDataTypeUsage returns the USAGE clause value for a data type token
func GetDataTypeUsage(tok Token) string {
	if info, ok := dataTypeInfos[tok]; ok {
		return info.Usage
	}
	return ""
}
