# Syntax
## Tokens

In the beginning, there was but tokens. A token is a
set of utf8 characters, separated by whitespace. The exception
being text within "double quotation marks," which are parsed as
one token.

## Expressions

Tokens are then strung together to form expressions. Expressions
are deduced via statement syntax

## Statements

Expressions are then combined into statements of many kinds.
Evaluated statements, declarative statements, and control statements.
Statements are separated by semicolons; or newlines

## Evaluated

Evaluated statements are simple enough. They are single expressions,
with nothing else.
```c++
1 + 4;

```

## Declarative

Declarative statements start with a typename token, then a token
for the name of the variable, then optionally a definition.

```c++
u8 number 5
// comments start with two slashes
/* or a slash with a star */
// u8 is unsigned integer 8 bit
```

## Control

Control statements run the gamut, but most are familiar. There's
`if`/`else`, `while` and `do`/`while`, as well as `for`.

```c++
if condition action
while condition action
do action while condition
for initial; condition; increment; action
// note how nothing actually requires paranthesis, and
// only for requires multiple statements
```

## Functions

Function declarations have paranthesis next to them with parameter names.
If you already know programming you probably know what this does.
```c++
u8 function(u8 param) {
  u8 variable param - 4
  returm variable * 2
}
u8 function2(u8 param, u8 param2) return param + 2 * param2
// The brakets are only required for multiple statements
```

## main()

The entrypoint is largely similar to c++, but with more type information.
```c++
int main(int argc, str[] argv){
  std::print(argv[0])
  return 0
}
//don't worry about int, str, print, or any of that
```