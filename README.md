# CompileBPV

A compiler for a fragment of ML based on Call-By-Push-Value. A detailed writeup about the phases of the compiler, as well as proofs of correctness, can be found in the [report](report.pdf).

# Usage 

Programs in this language are single ML expressions (within the subset we support). Examples can be found in the [tests directory](/test/).

To build the main executable, run `dune build ./bin/main.exe`

To compile a source file, run `dune exec ./bin/main.exe <filename>`


# Notes

- The version of OCaml used for this project was 5.1.1. The compiler may not build correctly on older/other versions

- The pretty printer uses unicode to match the notation used in the report. If the pretty printer appears weird in the terminal, that is probably why. The unicode characters used are: 𝕌, λ, and ∃. 

- The pretty printer prints code that is *slightly* different from the language grammar. In particular, the nullary split on unit, `Split(<>; .e)`, is printed as `Check(<>; e)`