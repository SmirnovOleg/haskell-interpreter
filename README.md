# haskell-course2019-hw
*Functional Programming course in Saint-Peterburg State University*

### Haskell Interpreter
    Run REPL with `make && make test`

**Implemented features:**
- Numbers, bools, characters, pairs
- Lists, strings
- Initial binary & unary operators
- Functions
    - Call-by-name evaluation strategy
    - Pointless style 
    - Partial application
- `if-then-else` & `where` expressions
- Simple pattern matching
- Simple Hindley-Milner type inference

**TODO**
- Fix parser:
    - `a ++ b` instead of `concat a b` calls
    - `(+ 2) 3`
    - `(f (+) 2) 1`
    - `(head [\x->x+1, \x->x+2]) 1`
