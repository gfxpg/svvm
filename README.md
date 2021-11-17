# Scalar/Vector Virtual Machine

**Svvm** is a proof-of-concept high-level shader assembler implemented as a Haskell EDSL.

## Features

1. Strongly-typed DSL with support for integer and FP32 operations
2. Descriptive type-level error messages via [TypeError](https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-TypeLits.html#g:4)
3. GFX-independent intermediate representation with virtual registers
4. Control-flow graph construction for further analysis both at IR and machine instruction level
5. Register allocation with alignment constraints using a variation of the linear scan algorithm

## Status

Several key issues were identified for the chosen approach:
1. While the frontend and the IR are well-typed, machine instruction
generation is not. This makes it hard to verify the backend's correctness
and prevent regressions.
2. The IR is too high-level/abstracted from the underlying hardware.
The ability to emit compound loads (`s_load_dwordxN`) automatically
is not worth the loss of fine control over the instructions that are generated.
While it is possible to write machine instructions directly in the DSL,
they are not typed (p.1), thus defeating the purpose a well-typed frontend—might
as well use a regular assembler at that point.

## Usage

### Prerequisites

1. [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)

2. (Optional) [Graphviz](https://graphviz.org/download)

### Assembling the sample shader

Navigate to the repository root and run `stack build`. When this command is executed for the first time,
Stack will download the compiler and build project dependencies. This may take a few minutes.

Next, execute `stack run`. The assembly for the sample shader will be printed out. In addition to that,
a graph showing all intermediate translation stages will be written to the `svvm-translation-graph.dot`
file in the current working directory.

If you have Graphviz installed, you can view it by running:
```sh
dot -Tsvg svvm-translation-graph.dot -o graph.svg && xdg-open graph.svg
```

### Modifying the sample shader

1. Open `app/Main.hs` and change `sampleShader`
2. Save the file and execute `stack run` (a rebuild will be triggered automatically)

### IDE support

* Visual Studio Code: [Haskell language support](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
