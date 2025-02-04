# roblox-c
C and C++ to Luau compiler for Roblox.

## Supports

### C with:
- Enums
- If statements
- Return
- Functions
- Calling functions
- Binary operators
- Unary operators
- Preprocessor
- Struct
- do-while loops
- Strings
- elseif
- For loops
- Casting
- else
- `*` & `&` (Pointers and Addresses)
- Unions
- `main` functions
- Gotos (Lua 5.2+)
- Short & Long values
- `++` & `--` (Increment and Decrement)
- Arrays
- Indexing (`[]`)
- switch (with fallthrough)
- Binary operators (`<<`, `>>`, `|`, `&`, `^`)

### C++ with:
- Everything in C
- Classes
- Constructors
- `delete`
- `new`
- Namespaces
- Destructors
- Inheritance

## Todo

- `->` (Pointer Member Access)
- Indexing (More robust array and member indexing)
- `this` (C++ `this` pointer)
- Roblox bindings (Directly interact with Roblox API from C/C++)
- Structs in unions (Nested structs and unions)

---

**Usage:**

```bash
rbxc.py [input_file.c or input_file.cpp] -o [output_file.luau] [options]
