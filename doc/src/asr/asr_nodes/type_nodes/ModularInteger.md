# ModularInteger

Represent modular integer, a `ttype` node.

## Declaration


### Syntax


```fortran
ModularInteger(int modulus_log_2, dimension* dims)
```


### Arguments


| Argument Name   | Argument Description                         |
|-----------------|----------------------------------------------|
| `modulus_log_2` | modulus, log_2; currently only 8, 16, 32, 64 |
| `dims`          | optional dimensions for a homogeneous array  |


## Description


The ttype **ModularInteger** represents a modular integer of a given modulus.
Modular integers with positive modulus `m` take values from the mathematical set
${0, 1, \ldots, m-1}$. For example `(ModularInteger 16 [])` represents values in
${0, 1, \ldots, 65535}$, and is implemented as a 16-bit unsigned integer in
hardware -- a `uint16_t` in C.


Currently, only the moduli $2^8$, $2^{16}$, $2^{32}$ and $2^{64}$ are
implemented. Typical hardware directly implements these moduli as 8‑bit, 16‑bit,
32‑bit and 64‑bit unsigned integers. These are thus the types that yield the
highest efficiency for modular arithmetic.


These types shall be specified as follows:


* `(ModularInteger 8 [])`

  - Optional verbose mode: `(ModularInteger :modulus_log_2 8 :dims [])`

* `(ModularInteger 16 [])`

  - Optional verbose mode: `(ModularInteger :modulus_log_2 16 :dims [])`

* `(ModularInteger 32 [])`

  - Optional verbose mode: `(ModularInteger :modulus_log_2 32 :dims [])`

* `(ModularInteger 64 [])`

  - Optional verbose mode: `(ModularInteger :modulus_log_2 64 :dims [])`


## Pitfalls


### Wrap-Around


Modular integers may have unintended behaviors near zero. Negative integers wrap
around and produce anomalies like  $(3 - 5) > 1 \mod n$:


```c
uint16_t x = (3 - 5);
printf("%d\n", x > 1);  // prints "1" for true
```

>     1


The origin of apparently anomalous behaviors is with overloaded arithmetic
operations like binary minus, and with comparison operators like `>`.
Programmers must be thoughtful about these operators with all computer
arithmetic, and especially with modular integers.


Modular integers represent finite subsets of the mathematical integer set ℕ, ℕ
and all its subsets contain no negatives. Therefore, subtraction of modular
integers always produces positive numbers.


Negative aliases for members of a set of modular integers are permitted, but
they are just aliases for positive numbers. For example, $-5 \mod 2^8$ is an
alias for $251$, and


```c
uint8_t y = -5;
printf("y == 251: %d\n", y == 251);  // prints "1" for true
```

>     y == 251: 1


Machine-limited integers like C's `int8_t`, `int16_t`, etc. are finite subsets
of the mathematical set ℤ, which contains all integers: positive, negative, and
zero. Computer BigInts represent all members of ℤ, with the only limitation
being practical, implementation-dependent limits of memory.


### Loop Variables


Loop variables can produce bad array indices. Consider the following, which
iterates over the entire positive range of `size_t` (presumably $2^{32}$)
instead of stopping after the obviously desired number of eight iterations:


```c
// "8" is a cardinal number; "size_t" is appropriate
float xs[((size_t)8)];
// "i" is an ordinal number; "size_t" not appropriate
for (size_t i = 7; i >= 0; i--)
    printf("xs[%u]: %d\n", i, xs[i]);
```

>     ...
>     xs[4294963194]: 0
>     xs[4294963193]: 65794
>     xs[4294963192]: 1179403647
>     signal: segmentation fault (core dumped)


That code ultimately produces a `segfault` when the index $i=0$ is decremented
and counts down from high memory after the ninth iteration.


Mathematically, the error comes from typing the loop index as a modular integer,
`size_t`. This type is suitable for the size argument of the array declaration
`xs[((size_t)8)]`. The size argument `8` is a
[cardinal number](https://en.wikipedia.org/wiki/Cardinal_number). In the context
of the loop, however, the array index `[i]` is an
[ordinal number](https://en.wikipedia.org/wiki/Ordinal_number) and must be
allowed negative values. The following is corrected code:


```c
// "8" is a cardinal number; "size_t" is appropriate
float xs[((size_t)8)];
// "i" is an ordinal number; "int" is appropriate
for (int i = 7; i >= 0; i--)
    printf("xs[%u]: %d\n", i, xs[i]);
```


>     xs[8]: 0
>     xs[7]: 0
>     xs[6]: 0
>     xs[5]: 0
>     xs[4]: 0
>     xs[3]: 0
>     xs[2]: 0
>     xs[1]: 0
>     xs[0]: 0


In cases where a lower-precision signed index does not have enough range to
cover all positive indices of an array, the signed index of the next higher
power of 2 may be necessary. For example, an index of type `int8_t` does not
suffice to cover an array with more than 127 elements. One must use an index of
type `int16_t` or larger.


### Negative Arguments of the Mod Operator


Various programming languages have varying treatments of
[the modulo operation](https://en.wikipedia.org/wiki/Modulo) for negative
arguments. ASR must handle all of them in a disciplined way **(TODO)**.


## Restrictions


An ASR loop variable may not be a `ModularInteger`. `ModularInteger` is coerced
to C unsigned integers in a `BindC` context.


## References


[Modular Arithmetic, Wikipedia](https://en.wikipedia.org/wiki/Modular_arithmetic)


## Future


In the future, ASR may support other useful powers of two:

* `(ModularInteger 1 [])` one bit

* `(ModularInteger 2 [])`

* `(ModularInteger 4 [])` nibble, one hex digit, half of an `octet`


Also, in the future, arbitrary moduli may be supported, but only via
the verbose syntax, as in

* `(ModularInteger :modulus 42, :dims [])`

* `(ModularInteger :prime_modulus 43, :dims[])`


| Future          |                           |
|-----------------|---------------------------|
| `modulus`       | any non-zero integer      |
| `prime_modulus` | any positive prime number |


## Operations


The operations defined on modular integers are:


* Binary arithmetic operations (`+`, `-`, `*`, `/`, `**`) and unary minus. The
  operations are defined as if on signed integers of infinite extent, and then
  the modular residual is taken, so that the value is in the double-closed
  interval $[0..(\textrm{modulus}-1)]. Both arguments must be `ModularInteger`
  and the result is a `ModularInteger`.


* Comparison operations: `<`, `>`, `==`, `>=`, `<=`, `/=`, defined as if first
  casting to signed integers of infinite extent, then comparing. The result is a
  `Logical`.


* Casting to and from `BitVector` and `Integer`. The casting does not change
  bit content, just interpretation.


Bitwise operations are not supported. Cast to `BitVector` for bitwise
operations. Casting `ModularInteger` to `BitVector` and back is a NOP at run
time.


## Use Cases


### High-Level versus Low-Level


For most high-level arithmetic code, aside from applications of number theory,
signed integers are almost always desirable.


Modular integers with power-of-two moduli are efficient in hardware. It may be
useful or necessary to cast signed integers to modular integers when performing
low-level operations, and then, even to `BitVector`. Such casts have no speed
penalties at run time.


For example, it is well known that an efficient way to multiply a small number
by two is to shift its bit representation to the left by one position. In
ASR-C-pseudocode, one might write


```c
typedef uint16_t bitvector16_t;

int main(void) {
  int x = -5;  // <~~~ let's multiply by 2 efficiently
  bitvector16_t y = ((bitvector16_t)x);
  bitvector16_t z = (y << 1);
  int zi = ((int16_t) z);
  printf("zi: %d, (-5 << 1) == -10: %d\n", zi, AI == -10);
}
```


>     zi: -10, (-5 << 1) == -10: 1


### Modular Arithmetic


Modular arithmetic two use cases:

1. actual modular arithmetic when the modulus is power of two (for now), as in
   crypto, telecomm, digital logic, etc., when wrap-around is the desired
   behavior

2. a finite interval of `Nat` that includes `0`, for instance `(0, 1, ...,
   255)`. Such intervals are cardinal numbers for declaring sizes of arrays (for
   example), but not for iterating over them. For iterating, use a signed int to
   model an ordinal number because you want to catch overflow errors in Debug
   mode.


### Interfacing with Low-Level Code


Hardware typically represents all modular integers and signed integers as
unsigned. Operations like `printf` or the comparison operators like `<=` may
treat them as signed. Operations like bit-shift may treat them as bit vectors.
The next example with C bit fields shows how assignment is performed unsigned
$\mod 8$ but printing is performed in $\dsZ_{2^{32}}$, signed arithmetic modulo
$2^{32}$.


Programmers must deeply understand the definitions of machine arithmetic,
comparison operations, and bit operations and must choose the types of arguments
to low-level procedures with utmost care.


### C Bit Fields


C compilers will generate code for many moduli that are not $2^8$, $2^{16}$,
$2^{32}$ or $2^{64}$. Consider the following example, which shows how Unix-like
octal permission codes might be modeled in a C struct with bit fields. This
example exhibits arithmetic wrap-around modulo 8, and interpretation of the same
3‑bit numbers as signed, 32‑bit numbers on printout:


```c
#include <stdio.h>
#include <stdint.h>

typedef
struct unix_permissions {
    int world: 3;
    int group: 3;
    int owner: 3;
} perms, * pperms;

void chmod(pperms pperms_output,
           uint8_t world,
           uint8_t group,
           uint8_t owner) {
    pperms_output->world = world;
    pperms_output->group = group;
    pperms_output->owner = owner;
}

void print_perms (const pperms pperms) {
    printf("octal (bit-field arithmetic in 3 bits):\n"
      "  perms.world: %o\n  perms.group: %o\n  perms.owner: %o\n\n",
        pperms->world, pperms->group, pperms->owner);
    printf("signed decimal (printout in 32 bits):\n"
      "  perms.world: %d\n  perms.group: %d\n  perms.owner: %d\n\n",
        pperms->world, pperms->group, pperms->owner);
}

int main(void) {
  perms perms_bad = {.world = 42, .group = 43, .owner = 57};
  print_perms(& perms_bad);
  perms perms_good = {.world = 7, .group = 5, .owner = 5};
  print_perms(& perms_good);
}
```


>     octal (bit-field arithmetic in 3 bits):
>       perms.world: 2
>       perms.group: 3
>       perms.owner: 1
>
>     signed decimal (printout in 32 bits):
>       perms.world: 2
>       perms.group: 3
>       perms.owner: 1
>
>     octal (bit-field arithmetic in 3 bits):
>       perms.world: 37777777777
>       perms.group: 37777777775
>       perms.owner: 37777777775
>
>     signed decimal (printout in 32 bits):
>       perms.world: -1
>       perms.group: -3
>       perms.owner: -3



### Mathematical Integers


When making finite approximations of mathematical integers, use `Integer`.
Overflow is a runtime error (in Debug mode). When overflow happens, increase the
size.

Practically speaking, the type of an integer index in a `for` loop should always
be an `i32` or `i64`.


For integers that wrap around without overflow, tied to their bit
representation, use `ModularInteger` with `modulus=2^n_bits`. For example if you
need a 5‑bit integer that wraps around, use `(ModularInteger 5 [])` (verbose:
`(ModularInteger :modulus_log_2=5 :dims [])`).


When needing bitwise operations, use or cast to `BitVector`.


You can always cast between the three representations without run-time overhead,
so choose your main representation based on what you need the most often.


## Examples


#### Python code:


```python
x: u16
y: u64
```


####ASR:


```clojure
(TranslationUnit
    (SymbolTable
        1
        {
            :x
                (Variable
                    1
                    x
                    []
                    Local
                    ()
                    ()
                    Default
                    (ModularInteger 65536 [])
                    Source
                    Public
                    Required
                    .false.
                ),
            :y
                (Variable
                    1
                    y
                    []
                    Local
                    ()
                    ()
                    Default
                    (ModularInteger 18446744073709551616 [])
                    Source
                    Public
                    Required
                    .false.
                )
        })
    []
)
```


## See Also


[LogicalNot](logicalnot.md)
