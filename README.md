# Hawat Programming Language Interpreter

An interpreter for the Hawat programming language, implemented in Haskell. Hawat is a statically-typed functional programming language that supports functions, lambdas, control flow, and basic data types.

## About Hawat

Hawat is a programming language featuring:

- **Static typing** with type inference
- **Basic types**: `int`, `string`, `bool`, `void`
- **Function types**: `(Type1, Type2) -> ReturnType`
- **Array types**: `[Type]` *(planned for future implementation)*
- **Lambda expressions** with closures
- **Control flow**: if/else, while loops, for loops
- **Loop control**: break and continue statements
- **Built-in functions**: `printInt()`, `printStr()`, and more

### Example Program

```hawat
fun factorial (int n) -> int {
    if (n <= 1) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

fun main () -> int {
    printInt(factorial(5)); printStr("\n"); // Output: 120
    return 0;
}
```

### Lambda Example

```hawat
fun main () -> int {
    (int) -> int double = lambda(int x) -> int {return 2 * x;};
    printInt(double(5)); printStr("\n"); // Output: 10
    return 0;
}
```

## Project Structure

```
hawat/
├── src/                    # Source code
│   ├── Hawat.cf           # Grammar definition (BNF format)
│   ├── Main.hs            # Main entry point
│   ├── Interpreter.hs     # Program interpreter
│   ├── InterpreterEnv.hs  # Interpreter environment
│   ├── InterpreterError.hs # Interpreter error handling
│   ├── Typechecker.hs     # Static type checker
│   ├── TypecheckerEnv.hs  # Typechecker environment
│   ├── TypecheckerError.hs # Typechecker error handling
│   ├── Utils.hs           # Utility functions
│   └── Parser/            # Generated parser files (created by BNFC)
├── good/                  # Test programs that should succeed
├── bad/                   # Test programs that should fail
├── Makefile              # Build configuration
├── run_tests.py          # Test runner script
└── README.md            # This file
```

## Prerequisites

- **GHC** (Glasgow Haskell Compiler)
- **BNFC** (BNF Converter) - for generating the parser from grammar
- **Make** - for building the project
- **Python 3** - for running tests

### Installing Dependencies

On Ubuntu/Debian:
```bash
sudo apt-get install ghc bnfc make python3
```

On macOS with Homebrew:
```bash
brew install ghc bnfc make python3
```

## Building the Project

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd hawat
   ```

2. Build the interpreter:
   ```bash
   make
   ```

This will:
- Generate parser files from the grammar (`src/Hawat.cf`) using BNFC
- Compile the Haskell source code
- Create the `interpreter` executable

## Usage

### Running Hawat Programs

**From file:**
```bash
./interpreter program.hwt
```

**From stdin:**
```bash
./interpreter < program.hwt
# or
echo 'fun main() -> int { printStr("Hello World\n"); return 0; }' | ./interpreter
```

**Help:**
```bash
./interpreter --help
```

### Program Structure

Every Hawat program must have a `main` function with signature `() -> int`:

```hawat
fun main () -> int {
    // Your code here
    return 0;  // Exit code
}
```

## Running Tests

The project includes a comprehensive test suite:

```bash
python3 run_tests.py
```

**Options:**
- `--interpreter path/to/interpreter` - Specify interpreter executable
- `--test_folder path/to/tests` - Specify test directory

### Test Structure

- **`good/`** - Programs that should execute successfully
- **`bad/`** - Programs that should fail (syntax errors, type errors, runtime errors)

Each test consists of:
- `*.hwt` - Source file
- `*.out` - Expected output

## Language Features

### Types

- `int` - Integer numbers
- `string` - Text strings  
- `bool` - Boolean values (`true`, `false`)
- `void` - Void type (no value)
- `(Type1, Type2) -> ReturnType` - Function types
- `[Type]` - Array types *(planned for future implementation)*

### Variables and Functions

```hawat
// Variable declarations
int x = 5;
string name = "Hawat";
bool flag = true;

// Function definition
fun add(int a, int b) -> int {
    return a + b;
}

// Arrays (planned for future implementation)
// [int] numbers = [1, 2, 3, 4, 5];
// printInt(numbers[0]); // Access element
```

### Control Flow

```hawat
// If statements
if (condition) {
    // code
} else if (other_condition) {
    // code  
} else {
    // code
}

// While loops
while (x < 10) {
    x++;
}

// For loops
for (i = 0 to 9) {
    printInt(i);
}

// Break and continue
while (true) {
    if (condition) break;
    if (other_condition) continue;
    // code
}
```

### Lambda Functions

```hawat
fun main() -> int {
    // Lambda with closure
    int multiplier = 3;
    (int) -> int multiply = lambda(int x) -> int {
        return x * multiplier;
    };
    
    printInt(multiply(5)); // Output: 15
    return 0;
}
```

## Error Handling

The interpreter provides detailed error messages for:

- **Parse errors** - Invalid syntax
- **Type errors** - Type mismatches, undeclared variables
- **Runtime errors** - Division by zero, etc.

## Clean Up

```bash
make clean      # Remove build files
make distclean  # Remove build files and executable
```
