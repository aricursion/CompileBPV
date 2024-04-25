# CompileBPV

To build the main executable, run `dune build ./bin/main.exe`

To compile a source file, run `dune exec ./bin/main.exe <filename>`

# Caveats 

- The pretty printer uses unicode to match the notation used in the report. If the pretty printer appears weird in the terminal, that is probably why. The unicode characters used are: 𝕌, λ, and ∃. 

- The pretty printer prints code that is *slightly* different from the language grammar. In particular, the nullary split on unit, `Split(<>; .e)`, is printed as `Check(<>; e)`