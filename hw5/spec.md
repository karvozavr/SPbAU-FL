# Language `L` syntax specification.

- Whitespace symbols & indentation has no meaning. I.e. program with all deleted whitespace symbols is correct and equivalent to original.

- Block 
```c++
statement; 
statement;
statement;
...
```

- All arithmetic assotiativity should be written explicitly:
```c++
print(2 * ((3 - 1) + 2))
```

- Any statement in block should be ended with `;`
```c++
x = 20;
if (a > b) then {
    write(a);
    read(b);
} else {
    do a barrel roll;
};
```

- Highest level constructions are function definitions only!
```c++
function_name0(ident, ident, ...) {
    function body
}
function_name1(ident, ident, ...) {
    function body
}
function_name2(ident, ident, ...) {
    function body
}
main() {
    function body
}
```

- Function definitions:
```c++
function_name(arg1name, arg2name, ...) {
    block
}
```
or 
```c++
function_name() {
    block
}
```

- Function call:
```c++
function_name(expr, expr, ...);
```
or
```c++
function_name();
```

- Conditional: 
```c++
if (expr) {
    block
} else {
    block
};
```

- `while` loop:
```c++
while (expr) do {
    block
};
```

- IO functions:
```
write(expr);
read(identificator);
```

#Syntax shugar

- Operator-assignment:
```
a += b;
c -= d;
e *= f;
g /= h;
```
`a += b` is desugaring to `a = a + b`.

- No else branch. 

```
if (a > b) {
    do();
} fi;
```
is desugating to
```
if (a > b) {
    do();
} else {}
```
