# GoBOL Command-Line Interface

## Overview

The `cmd` package provides the command-line interface for the GoBOL compiler. It handles command-line argument parsing, compiler configuration, and orchestrates the compilation process through all phases.

## Command Structure

### Main Commands

```bash
gobol [global-options] <command> [command-options] [arguments]
```

#### Primary Commands
- `compile` - Compile COBOL source files
- `run` - Compile and execute COBOL program
- `check` - Syntax and semantic checking without compilation
- `fmt` - Format COBOL source files
- `doc` - Generate documentation from COBOL source
- `init` - Initialize a new COBOL project
- `version` - Display version information

#### Development Commands
- `dump-ast` - Output AST for debugging
- `dump-ir` - Output IR for debugging
- `tokens` - Display token stream
- `profile` - Performance profiling
- `test` - Run COBOL test suites

## Command-Line Options

### Global Options
```bash
--verbose, -v       # Increase verbosity level
--quiet, -q         # Suppress non-error output
--config=<file>     # Use specific config file
--color=(auto|on|off) # Control color output
--debug            # Enable debug output
--version         # Show version information
--help, -h        # Show help information
```

### Compile Options
```bash
--output, -o <file>     # Output file
--target=<platform>     # Target platform
--opt-level=(0|1|2|3)   # Optimization level
--debug-info           # Include debug information
--listing              # Generate listing file
--cross-reference      # Generate cross-reference
--source-format=(fixed|free) # Source format
--charset=<encoding>   # Source file encoding
```

### Run Options
```bash
--args="..."          # Program arguments
--workdir=<dir>       # Working directory
--env=<key=value>     # Environment variables
--profile            # Enable runtime profiling
--trace              # Enable execution tracing
--memory-limit=<size> # Set memory limit
```

## Implementation Plan

### Phase 1: Basic CLI Framework
1. Implement command-line parsing
2. Add basic commands (compile, version)
3. Set up logging framework
4. Implement configuration loading
5. Add help system
6. Create basic error reporting

### Phase 2: Compiler Integration
1. Implement compilation pipeline
2. Add source file handling
3. Implement output generation
4. Add basic optimization flags
5. Implement debug info generation
6. Add listing file generation

### Phase 3: Advanced Features
1. Add format command
2. Implement documentation generator
3. Add test runner
4. Implement profiling
5. Add development tools
6. Create project initialization

### Phase 4: Runtime Support
1. Implement run command
2. Add program argument handling
3. Implement environment setup
4. Add runtime configuration
5. Implement execution tracing
6. Add performance monitoring

## Configuration

### Configuration File (gobol.yaml)
```yaml
compiler:
  source-format: fixed
  charset: utf-8
  optimization: 2
  debug-info: true
  warnings-as-errors: false

output:
  directory: ./bin
  listing: true
  cross-reference: true
  source-map: true

runtime:
  memory-limit: 1G
  stack-size: 8M
  thread-limit: 4
  
project:
  name: my-cobol-app
  version: 1.0.0
  target: linux-amd64
  dependencies:
    - lib: standard-io
      version: ^1.0.0
```

## Project Structure

```
myproject/
├── gobol.yaml        # Project configuration
├── src/
│   ├── main.cbl     # Main program
│   └── modules/     # Subprograms
├── copybooks/       # COPY members
├── tests/          # Test files
├── data/           # Data files
└── bin/            # Output directory
```

## Exit Codes

```go
const (
    ExitSuccess          = 0
    ExitCompileError    = 1
    ExitRuntimeError    = 2
    ExitConfigError     = 3
    ExitUsageError      = 4
    ExitSystemError     = 5
)
```

## Progress Reporting

### Compilation Progress
```
GoBOL Compiler v1.0.0
[1/6] Parsing source files...
[2/6] Performing semantic analysis...
[3/6] Generating intermediate code...
[4/6] Optimizing...
[5/6] Generating target code...
[6/6] Writing output files...
Compilation completed in 2.3s
```

### Error Output
```
Error: source.cbl:123 - Undefined data item 'CUSTOMER-RECORD'
  | 
123|     MOVE CUSTOMER-RECORD TO PRINT-LINE
  |          ^^^^^^^^^^^^^^
  = Suggestion: Did you mean 'CUSTOMER-REC'?

Compilation failed with 1 error
```

## Implementation Details

### Command Handler Interface
```go
type Command interface {
    Name() string
    Description() string
    Run(args []string) error
    SetFlags(*flag.FlagSet)
}
```

### Compiler Driver
```go
type Driver struct {
    Config     *Config
    ErrorLog   *log.Logger
    Progress   *ProgressReporter
    FileSystem fs.FS
}

func (d *Driver) Compile(sources []string) error
func (d *Driver) Run(program string, args []string) error
func (d *Driver) Check(sources []string) error
```

## Testing Strategy

### Unit Tests
- Command-line parsing
- Configuration loading
- Progress reporting
- Error handling
- Exit code management

### Integration Tests
- End-to-end compilation
- Configuration scenarios
- Error scenarios
- Output generation
- Tool integration

### System Tests
- Cross-platform testing
- Large program compilation
- Performance benchmarks
- Resource usage monitoring

## Performance Goals

- Command startup < 100ms
- Configuration loading < 50ms
- Basic compilation < 1s
- Full optimization < 5s
- Memory usage < 512MB

## Future Enhancements

### 1. IDE Integration
- Language server protocol
- Debug adapter protocol
- Project management
- Refactoring tools

### 2. Build System Integration
- Make integration
- CMake support
- Continuous integration
- Dependency management

### 3. Advanced Features
- Incremental compilation
- Distributed compilation
- Plugin system
- Custom toolchains

### 4. Developer Tools
- Performance profiling
- Memory analysis
- Coverage reporting
- Static analysis

## Security Considerations

1. Input Validation
   - Source file validation
   - Configuration validation
   - Command-line argument sanitization

2. Resource Management
   - File access controls
   - Memory limits
   - CPU usage limits
   - Temporary file handling

3. Environment Security
   - Working directory isolation
   - Environment variable handling
   - Dependency verification
   - Plugin sandboxing
