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
    - Test lambdas application -- ((f 2) 3)
    - Testing!
    - Types and type-definitions
- Interpreter
    - User-lambdas
    - Pattern-matching
    - Type checking
    - Type inference