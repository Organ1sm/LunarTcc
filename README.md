# Lunar Tiny C Compiler

Just a toy compiler.
The purpose of the project is to help me learn the principles of compilation, and the modular design is inspired by LLVM.

Under development....

## Goals
* Being able to compile **C Source Code** into assembly code.
* Implement few independent optimizations.

## Build
```
mkdir build
** use make 
    cmake -S. -Bbuild
** use Ninja
    cmake -S. -Bbuild -GNinja

make / Ninja
```

## Usage
 **Cat algorithm-gcd.c**  is 
```c
int gcd(int a, int b)
{
    int R;

    while ((a % b) > 0)
    {
        R = a % b;
        a = b;
        b = R;
    }

    return b;
}
```
#### Tokens Dumping
```
LunarTcc tests/examples/algorithm-gcd.c -dump-tokens
```
Output:
```
"int", Line: 1, Col: 1
"gcd", Line: 1, Col: 5
"(", Line: 1, Col: 8
"int", Line: 1, Col: 9
"a", Line: 1, Col: 13
",", Line: 1, Col: 14
"int", Line: 1, Col: 16
"b", Line: 1, Col: 20
")", Line: 1, Col: 21
"{", Line: 2, Col: 1
"int", Line: 3, Col: 5
"R", Line: 3, Col: 9
";", Line: 3, Col: 10
"while", Line: 5, Col: 5
"(", Line: 5, Col: 11
"(", Line: 5, Col: 12
"a", Line: 5, Col: 13
"%", Line: 5, Col: 15
"b", Line: 5, Col: 17
")", Line: 5, Col: 18
">", Line: 5, Col: 20
"0", Line: 5, Col: 22
")", Line: 5, Col: 23
"{", Line: 6, Col: 5
"R", Line: 7, Col: 9
"=", Line: 7, Col: 11
"a", Line: 7, Col: 13
"%", Line: 7, Col: 15
"b", Line: 7, Col: 17
";", Line: 7, Col: 18
"a", Line: 8, Col: 9
"=", Line: 8, Col: 11
"b", Line: 8, Col: 13
";", Line: 8, Col: 14
"b", Line: 9, Col: 9
"=", Line: 9, Col: 11
"R", Line: 9, Col: 13
";", Line: 9, Col: 14
"}", Line: 10, Col: 5
"return", Line: 12, Col: 5
"b", Line: 12, Col: 12
";", Line: 12, Col: 13
"}", Line: 13, Col: 1
```
#### AST Dumping

```
LunarTcc tests/examples/algorithm-gcd.c -dump-ast
```

Output:
```
TranslationUnit
  FunctionDeclaration 'int (int, int)' 'gcd'
    FunctionParameterDeclaration 'int' 'a'
    FunctionParameterDeclaration 'int' 'b'
    CompoundStatement
      VariableDeclaration 'int' 'R'
      WhileStatement
        BinaryExpression 'int' '>'
          BinaryExpression 'int' '%'
            ReferenceExpression 'int' 'a'
            ReferenceExpression 'int' 'b'
          IntegerLiteralExpression 'int' '0'
        CompoundStatement
          ExpressionStatement
            BinaryExpression 'int' '='
              ReferenceExpression 'int' 'R'
              BinaryExpression 'int' '%'
                ReferenceExpression 'int' 'a'
                ReferenceExpression 'int' 'b'
          ExpressionStatement
            BinaryExpression 'int' '='
              ReferenceExpression 'int' 'a'
              ReferenceExpression 'int' 'b'
          ExpressionStatement
            BinaryExpression 'int' '='
              ReferenceExpression 'int' 'b'
              ReferenceExpression 'int' 'R'
      ReturnStatement
        ReferenceExpression 'int' 'b'
```

#### IR Dumping
```
LunarTcc tests/examples/algorithm-gcd.c -dump-ir
```

Output:
```
func gcd ($a :i32, $b :i32) -> i32 :
.entry_gcd:
        salloc  $0 :i32
        store   [$0], $a
        salloc  $1 :i32
        store   [$1], $b
        salloc  $2 :i32
        j       <loop_header0>
.loop_header0:
        load    $3, [$0]
        load    $4, [$1]
        mod     $5, $3, $4
        cmp.ne  $6, $5, 0
        br      $6, <loop_end0>
.loop_body0:
        load    $7, [$0]
        load    $8, [$1]
        mod     $9, $7, $8
        store   [$2], $9
        load    $10, [$1]
        store   [$0], $10
        load    $11, [$2]
        store   [$1], $11
        j       <loop_header0>
.loop_end0:
        load    $12, [$1]
        ret     $12

```
