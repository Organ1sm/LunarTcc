# PreProcessor Support
- [x] `#include`
```c++
#include "test.h"
#include "../include/test.h"
```
- [x] `#define` 
```c++
#define N 100
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
```
- [x] `__LINE__`
```c++
int test(int a)
{
	unsigned res = __LINE__; 
	return res;  // 3
}
```

# Types and variables
+ [x] `char` , `unsigned char`
+ [x] `short`, `unsigned short`
+ [x] `int`, `unsigned int`
+ [x] `long` , `unsigned long`
+ [x] `long long`,  `unsigned long long` (64bit)
+ [x] `enum`
```c++
enum { A, B, C, D };
```
- [x] `struct`
- [x] `ND-array`
- [ ] `global-variale` `global-array`

## Type Qualifiers
- [x] `const`
- [x] `typedef`

## Literal
- [x] `Integer-Literal`, `Integer-Literal-Suffix`(`u`, `ul`, `l`, `ll`, `ull`)
- [x] `Character-Literal`

## Statements
- [x] `if-else`
- [x] `switch-case-default`
- [x] `for`
- [x] `break`
- [x] `continue`

## Comment
- [x] `single comment`
- [ ] `multiline comment`

# Operator

### Member Access Operator
- [x] `*`(Addressof) , `&`(DeRefrence)
- [x] `[]`
- [x] `->`
- [x] `.`

```cpp
a[b]
 *a
 &a
a->b
a.b
```

### Increment, Decrement Operator
- [x] `++`, `--`
```cpp
a++
a--
++a
--a
```

### Arithmetic Operator
- [ ] `+`
- [x] `-`
- [x] `+`, `-` 
- [x] `*`, `/` ,`%`
- [x] `~`
- [x] `&`, `^`
- [ ] `|`
- [x] `<<`, `>>`
```c++
 +a      // not support
 -a
a + b 
a - b
a * b
a / b
a % b

 ~a      
a & b
a | b    // not support
a ^ b
a >> b
a << b
```

### Assignment Operator
- [x] `=`,`+=`, `-=`, `*=`, `/=`
```c++
a = b
a += b
a -= b
a *= b
a /= b
```

### Comparison Operator
- [x] `<`, `>`, `<=`, `>=`, `!=`, `==`
```cpp
a < b
a > b
a <= b
a >= b
a != b
a == b
```


### Logical Operator
- [x] `!`
- [x] `&&`, 
- [x] `||`
```cpp
  !a
a && b
```

#### Other Operator
- [x] `a > b ? a : b`
- [x] `sizeof`
- [ ] `_Alignof`
