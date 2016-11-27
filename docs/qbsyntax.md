# QB Source Syntax

GH3 scripts are vaguely similar to Lua in syntax, though there are a few
"gotchas".  The syntax is clunky in places; this is somewhat intentional as we
don't have full information on the scripts, so the syntax was designed to
preserve the underlying features as much as possible so they can all be used.

The simplest possible script is one that does nothing:

```
script()
endscript
```

All blocks, including the script block, begin and end with keywords; unlike Lua,
the various "end" markers are different keywords to each other.

**New lines matter**.  Line continuations are not currently implemented, so an
expression split over multiple lines will generally be a parse error unless it
is inside the definition of a struct or a map.

Comments are as in ANSI C: `//` for line comments and `/* */` for block comments

## Values

GH3 scripts support a pretty standard collection of value types: decimal integers,
hexadecimal integers[\*](#hexintegers), floating point values, vectors of two or
three floating point values, ASCII strings, unicode strings, maps
(aka lookups, associative arrays, dictionaries...), arrays, structs and QbKeys
(names or checksums of things).

### Numeric values

Decimal and hexadecimal integers are as normal: 1234 and 0x1234 respectively.
Floating point numbers **must** have either the decimal point or an exponent:
3.0 or 3e8 for example.  Vectors are enclosed in parentheses: `(1.0, 2.0)`;
`(1.0, 2.0, 3.0)`.  Again, the decimal point or exponent are mandatory.

### QbKeys

QbKeys are globally scoped by default when used as a value: they reference some
globally-available game setting.  Assignment of a QbKey sets the key in the
*local* scope.  You can access locally-scoped QbKeys with a `%` prefix.

To access a QbKey where the checksum is known but the plaintext name is not, use
`$`.  For example, `$8CDC1683` is the same as the name `x`.  When combined with
`%`, the `%` comes first: `%$8CDC1683`.

### Strings

ASCII strings are enclosed in single quotes while unicode strings are in double
quotes.  Unicode strings only support UCS-2 (codepoints that fit in two bytes).

### Arrays

Arrays are in square brackets: `[ 1, 2, 3+4, f(x) ]`.  Members can be arbitrary
expressions.  Indexing into an array is `arr[3]`, for example.

### Maps

Maps are in curly braces, with comma-separated entries in the form `key: value`
where value can be an expression.

```
{
  id: %id,
  parent: root_container,
  just: [ left, top ]
}
```

There is a special map called "passthrough" denoted by `<...>` that contains all
the arguments your script was called with.  It can be extended like this:

```
{
  <...>,
  x: 1
}
```

### Structs

```
{
  type name = value;
  type name = value;
}
```

Structs are also enclosed in curly braces, but the entries are separated by
semicolon **and newline**.  The valid type keywords are:

* int
* float
* string
* wstring
* vector2
* vector3
* struct
* array
* qbkey
* qbkeyref
* stringptr
* stringqs

The exact purposes of the last few of these aren't yet documented; their
existence is taken from the source code to Nanook's Queen Bee tool.  The
corresponding values are exactly as above, except an array can no longer
contain expression results.  The last few values all take QbKey values.

## Control flow blocks

GH3 scripts support "if/elseif/else", "begin/repeat", and "switch" blocks.  All
are pretty similar to those in other languages, though "if" has no "then":

```
if condition
  doSomething()
elseif condition
  doSomethingElse()
else
  doAThirdThing()
endif

switch expr
case 1:
  doSomething()
case 2:
  doSomethingElse()
default:
  doAThirdThing()
endswitch

begin
  doSomething()
repeat (expr)
```

The main gotchas are that cases in a switch statement have no fallthrough and
do not use or support `break`.  The cases also only compare against "small"
literals: integers, hexadecimal integers or QbKeys.  The `(expr)` following
repeat is the number of times to repeat that block.  Any expression evaluating
to an integer is valid there.

Unlike the switch statement, the begin/repeat statement does support `break`.

## Expressions

The standard arithmetic, comparison and logical operators as found in C are
available; the binary operators are not.  In addition, "dereference" (`*`) is
a prefix operator that gets the value a QbKey refers to (we think; this isn't
100% confirmed).

Function calls are again as in C, but with keyword arguments optionally
available: `func(arg1, arg2, x=arg3)`.  You can mix keyword arguments with
non-keyword freely.  When `func` refers to a script, it calls the script with
those arguments.  When it refers to a native function, it creates a struct with
the arguments to pass in and passes that to the native (C++) code to evaluate.

A reference of available functions does not currently exist but will be
worked on soon.

Method calls are in the form `x:f(args)`.  Not much is known about these; they
appear to be predefined functions only.  One example is in
"create\_2d\_hud\_elements": `new_child_id:settags(morph=0)`.

<a name="hexintegers">*</a>: For some reason, there are different opcodes for
integers and hexadecimal integers in the compiled scripts.  Don't ask me why.
