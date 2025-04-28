# 🚀 GoBOL: Because COBOL Isn't Dead, It Just Smells Funny

[![Go Version](https://img.shields.io/badge/Go-1.21%2B-00ADD8.svg)](https://golang.org/doc/go1.21)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Status](https://img.shields.io/badge/status-it's%20complicated-orange.svg)](https://github.com/mattwebdev/gobol)

Yes, you read that right. I'm building a COBOL compiler. In Go. In 2025. Why? Because someone has to maintain those 240 billion lines of COBOL running the world's financial systems, and they might as well do it with modern tools.

## 📚 Documentation

- [Command Line Interface](cmd/README.md) - How to boss around the compiler
- [Internal Packages](internal/README.md) - Where COBOL goes to be transformed (like a digital spa day)
- [Token System](pkg/token/README.md) - The Rosetta Stone of our COBOL understanding
- [Error Handling](pkg/errors/README.md) - When things go wrong (and they will)
- [Testing Strategy](tests/README.md) - Proving it works (with science!)

## 🤔 What's This All About?

GoBOL is my attempt at a modern COBOL compiler that targets the ISO/IEC 1989:2014 standard, because apparently, COBOL had a standard update more recently than you thought. I'm bringing the joy of modern development tools to the language that refuses to die.

## ✨ Features (Already Implemented, Unlike Your Bank's System Upgrade)

- **Comprehensive Token System** - Full support for:
  - Report Writer tokens (because someone still uses those)
  - Condition handling (ON OVERFLOW, anyone?)
  - Data types (more than just PICTURE clauses!)
  - File organization keywords (as complex as your filing system)
  - Scope terminators (because even COBOL needs closure)
- **Smart Token Classification** - Each token knows its:
  - Category (from "arithmetic" to "usage", we've got them all)
  - Context (what works where, because COBOL is picky)
  - Class (because organization matters, even in chaos)
  - Aliases (THRU or THROUGH? Why not both!)
- **Modern Error Messages** - No more cryptic "ABORT ROUTINE 217" messages
- **Source Location Tracking** - Find bugs faster than your mainframe's boot time

## 🎯 Coming Soon

- **Optimized Code Generation** - Making COBOL run faster than your grandpa's stories about punch cards
- **Cross-Platform Support** - Run COBOL everywhere. Yes, even there. Why? Because we can
- **Full ISO/IEC 1989:2014 Compliance** - Because COBOL has standards (no, really!)

## 🏗️ Project Structure

```
.
├── cmd/
│   └── gobol/         # Where the magic begins
├── internal/
│   ├── lexer/        # Turns COBOL into something less COBOL-y
│   ├── parser/       # Tries to make sense of it all
│   ├── ast/          # Abstract Syntax Trees (very abstract, much syntax)
│   ├── semantic/     # Ensures your COBOL makes sense (good luck)
│   ├── ir/           # The middle ground between madness and method
│   ├── optimizer/    # Makes your code fast (relatively speaking)
│   └── codegen/      # Turns your COBOL into something computers understand
├── pkg/
│   ├── token/        # The Rosetta Stone of COBOL syntax (now with 100% more tokens!)
│   └── errors/       # For when things go wrong (and they will)
└── tests/            # Proving it actually works (with real tests!)
```

## 🛠️ Building

Requires Go 1.21 or later. Yes, I'm using a modern language to compile a not-so-modern one. Ironic, isn't it?

```bash
go build ./cmd/gobol
```

## 🚦 Usage

Check out our [Command Line Guide](cmd/cmd.md) for the full story, but here's the TL;DR:

```bash
gobol [options] input.cbl

# Example:
gobol your-grandfathers-legacy-code.cbl
```

## 🧪 Testing

We take testing seriously (unlike your bank's Y2K preparations). Check out our [Testing Strategy](tests/tests.md) for the full details.

```bash
go test ./...  # Run all tests
go test ./pkg/token/  # Test just the token package
```

## 🔧 Development

- [Token System Documentation](pkg/token/token.md) - How we make sense of COBOL
- [Error Handling Guide](pkg/errors/errors.md) - Because errors should be helpful

## 🤝 Contributing

Found a bug? Want to add a feature? Know what PROCEDURE DIVISION actually means? I'd love your help! Here's how:

1. Fork it
2. Branch it
3. Code it
4. Test it (yes, really, we have those now!)
5. PR it

Check our docs for detailed guidelines:
- [Testing Guide](tests/tests.md)
- [Error Handling](pkg/errors/errors.md)
- [Token System](pkg/token/token.md)

## 📜 License

MIT License - Because even COBOL deserves some freedom.

## 💭 Why Though?

Because someone had to do it, and it might as well be me. Plus, have you seen what banks are paying COBOL developers these days? 

## 🎯 Project Goals

1. Make COBOL development less painful
2. Bring modern tooling to legacy systems
3. Save developers from having to use ancient compilers
4. Prove that you can teach an old dog new tricks
5. Support every COBOL token known to mankind (and we're getting there!)

Remember: COBOL isn't dead, it's just resting its eyes while controlling the world's financial systems.

---
*Built with ❤️, 😅, and a fully-tested token system that understands COBOL better than most humans* 