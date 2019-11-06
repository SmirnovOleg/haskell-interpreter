# Haskell Interpreter


Description:
- AST implemented
- Initial parser/interpreter/REPL implemented; waiting for code review
- Run via `make && make test`


Implemented features:
- Numbers, bools, characters, pairs
- Lists, strings
- Initial binary & unary operators
- Functions
    - Call-by-name evaluation strategy
    - Works on lambdas
    - Pointless style 
    - Partial application
- `if-then-else` statements


TODO
- Parser
    - Update lambdas application -- ((f 2) 3)
    - Distinguish logic and ord operators
    - Testing!
    - Types and type-definitions
    - Inline expressions with `where` keyword
- Interpreter
    - User-lambdas
    - Pattern-matching
    - Type checking
    - Type inference