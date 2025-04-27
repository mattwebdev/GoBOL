# 🚀 GoBOL: Because COBOL Isn't Dead, It Just Smells Funny

[![Go Version](https://img.shields.io/badge/Go-1.21%2B-00ADD8.svg)](https://golang.org/doc/go1.21)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Status](https://img.shields.io/badge/status-it's%20complicated-orange.svg)](https://github.com/mattwebdev/gobol)

Yes, you read that right. I'm building a COBOL compiler. In Go. In 2025s. Why? Because someone has to maintain those 240 billion lines of COBOL running the world's financial systems, and they might as well do it with modern tools.

## 🤔 What's This All About?

GoBOL is my attempt at a modern COBOL compiler that targets the ISO/IEC 1989:2014 standard, because apparently, COBOL had a standard update more recently than you thought. I'm bringing the joy of modern development tools to the language that refuses to die.

## ✨ Features (Planned, Just Like Your Bank's System Upgrade)

- **Full ISO/IEC 1989:2014 Compliance** - Because COBOL has standards (no, really!)
- **Modern Error Messages** - No more cryptic "ABORT ROUTINE 217" messages
- **Source Location Tracking** - Find bugs faster than your mainframe's boot time
- **Optimized Code Generation** - Making COBOL run faster than your grandpa's stories about punch cards
- **Cross-Platform Support** - Run COBOL everywhere. Yes, even there. Why? Because I can.

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
│   ├── token/        # Defines what COBOL actually is (still figuring it out)
│   └── errors/       # For when things go wrong (and they will)
└── test/             # Proving it actually works
```

## 🛠️ Building

Requires Go 1.21 or later. Yes, I'm using a modern language to compile a not-so-modern one. Ironic, isn't it?

```bash
go build ./cmd/gobol
```

## 🚦 Usage

```bash
gobol [options] input.cbl

# Example:
gobol your-grandfathers-legacy-code.cbl
```

## 🤝 Contributing

Found a bug? Want to add a feature? Know what PROCEDURE DIVISION actually means? I'd love your help! Here's how:

1. Fork it
2. Branch it
3. Code it
4. Test it (yes, really)
5. PR it

## 📜 License

MIT License - Because even COBOL deserves some freedom.

## 💭 Why Though?

Because someone had to do it, and it might as well be me. Plus, have you seen what banks are paying COBOL developers these days? 

## 🎯 Project Goals

1. Make COBOL development less painful
2. Bring modern tooling to legacy systems
3. Save developers from having to use ancient compilers
4. Prove that you can teach an old dog new tricks

Remember: COBOL isn't dead, it's just resting its eyes while controlling the world's financial systems.

---
*Built with ❤️, 😅, and a healthy dose of "why am I doing this again?"* 