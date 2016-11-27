# qbscript
(WIP) compiler and decompiler pair for GH3 in-game scripts

## Compiler
The compiler supports a large subset of the operations available in GH3
in-game scripts; you can find full details of the syntax [here][qbsyntax].

The operations not yet supported are:

* Random (`0x2F`)
* Random2 (`0x37`)
* RandomRange (`0x30`)
* RandomRange2 (`0x38`)
* RandomNoRepeat (`0x40`)
* RandomPermute (`0x41`)
* @ (`0x31`)
* useheap (`0x45`)
* #*\<string\>*

These are unimplemented for various reasons:

* The random operations have unknown operands
* The purpose of `@` and `#<string>` are both still unknown.
* The context in which `useheap` should appear is unknown

If you find a script using any of the above opcodes, please open an issue
or otherwise contact me so I can look into them!

## Decompiler
The decompiler will be coming soon.

[qbsyntax]: docs/qbsyntax.md
