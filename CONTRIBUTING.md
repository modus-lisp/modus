# Contributing to Modus

## Development Environment

### Required Tools
- SBCL (Steel Bank Common Lisp)
- QEMU
- Git

### Quick Start
```bash
git clone https://github.com/anthropics/modus.git
cd modus

# The run matrix (one entry point for every QEMU configuration)
./scripts/run.sh list

# Boot to serial REPL
./scripts/run.sh x64-repl

# ... or evaluate one expression and exit
./scripts/run.sh x64-repl "x"

# Boot with SSH
./scripts/run-x64-ssh.sh
```

## License

By contributing to Modus, you agree that your contributions will be licensed under its MIT License.
