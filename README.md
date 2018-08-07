# Third Assignment
## Short description
Develop a syntax-aware editor for programs in the language as de-veloped in Assignment 2. The editor shall be written in a staticallytyped functional language like Haskell and ML. In addition to theusual functionality of an editor (loading, showing, modifying and storing text), the editor shall highlight

- [ ] obvious syntactic errors like unbalanced braces,
- [ ] other occurrences of a variable or rule name currently pointedto by the cursor,
- [ ] and similar useful things depending on the language.

[...](aufgabe3.pdf)

## Usage

For starting the project switch to the root directory and run

```
foo@bar:~$ elm-reactor
elm-reactor 0.18.0
Listening on http://localhost:8000
```

The tests can be started with the following command inside the root directory

```
foo@bar:~$ elm-test
```