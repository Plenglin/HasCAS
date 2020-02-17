# HasCAS

Haskell Computer Algebra System (or HasCAS for short) is an experimental CAS I wrote as a learning project. My main goals are to:
1. Learn Haskell and functional programming methods
2. Learn how to work with Abstract Syntax Trees (ASTs) in simplifying and expanding expressions
3. Learn about optimization in factoring polynomials, solving equations, and solving integrals

## Features

Items marked with an asterisk (*) might require some sort of decision tree or optimizing AI.

- [x] Syntax trees
- [ ] Parsing trees from a string
- [ ] Substitution of variables
- [ ] Evaluating constant expressions
- [ ] Working with expressions
    - [ ] Expanding polynomials
    - [ ] Simplifying polynomials
    - [ ] Factoring polynomials
    - [ ] Combining non-algebraic terms
- [ ] Solving equations for x algebraically
    - [ ] Algebraic equations
        - [ ] ...with a single instance of x
        - [ ] ...with multiple instances of x
    - [ ] Transcendental equations
        - [ ] ...with a single instance of x
        - [ ] * ...with multiple instances of x
            - [ ] * ...by identifying whether or not it's even solvable in the first place
            - [ ] * ...by applying trig identities
- [ ] Solving systems of equations
    - [ ] Linear systems
        - [ ] Using linear algebra (I'll just pull in a library for this and spare myself the headache of row reduction)
    - [ ] * Nonlinear systems
- [ ] Calculus
    - [ ] Single-variable Calculus
        - [ ] Derivatives
        - [ ] Limits
            - [ ] ...of simple terms
            - [ ] * ...by identifying indeterminate forms and applying L'Hospital
        - [ ] Integration
            - [ ] ...of simple terms
            - [ ] * ...by substitution
            - [ ] * ...by parts
                - [ ] ...and identifying repeated terms
    - [ ] Multi-variable Calculus
        - Is going to be a bit of a headache
