# From session types to Rust
In this project, we create a translation between security session types, proverif syntax and Rust-code. We take advantage of session types for communication and describe the behaviour of each participant. In this project, we extend security session types further and introduce a translation to the Rust programming language and proverif syntax. The protocol itself will be translated into runnable rust code while symbolic functions will be compiled to unimplemented functions, that the developer will then have to take care of implementing.
The proverif syntax will be translated into a runnable file, that will be able to execute the queries in the .sst file, and a developer will be able to extend on it with more queries if needed.

## Building
It's as simple as running make. This will output a binary named *main.native*
```bash
$ make
```
## Usage
```bash
Usage: ./main.native <FILE> [COMMAND] 

Commands:
    rust        Compiles the given .sst file to rust syntax (default option)
    proverif    Compiles the given .sst file to proverif syntax
```
