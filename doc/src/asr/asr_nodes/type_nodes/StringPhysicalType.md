# String Physical Type

## Types 

```ASDL
string_physical_type = CChar | DescriptorString
```

## Description

- **CChar :** 
	- Corrsponds to C's type `char`.
    - Used for C interoperability only.
    - Used with function's arguments and return.
    - Used with bind C functions. To be accurate, A string-typed variable is set to have `CChar` physical type when it's : bindC variable + not local variable + kind=`c_char`
	- That means string expressions (other than `ASR::Var`) shouldn't be of physical type `CChar`, because expressions in Fortran code shouldn't be treated as if they're C expressions, hence we use any type other than the one made for C interoperability.    
- **DescriptorString :** 
	- It's an LLVM struct holding string's **data** and **length**.
	- It's represented in LLVM IR as : `{char*, int64}`.
    - It fits every string case in Fortran, The different cases means the length variations (assumed, deferred, compile-time-expression, runtime-expression).
    - It's great power comes when length must be represented at runtime, as the length could be deferred `character(:)` or a non-compile-time known expression `character(len=foo())`. That's why we encapsulated string's data and its length within a type which we name **string descriptor**.


## Usage 

- **DescriptorString**

	- It's could with every string type variation (length variations).
    - It's Used to hold information about the length at runtime.

```Fortran
character(:) , allocatable:: chr
```

```Clojure
chr:
    (Variable
        2
        chr
        []
        Local
        ()
        ()
        Default
        (Allocatable
            (String 1 () DeferredLength DescriptorString) ; Notice the phsyical type
        )
        ()
        Source
        Public
        Required
        .false.
        .false.
        .false.
        ()
        .false.
        .false.
    )

```
***
```Fortran
  character(10) :: str 
```

```clojure
str:
(Variable
    2
    str
    []
    Local
    ()
    ()
    Default
    (String 1 (IntegerConstant 10 (Integer 4) Decimal) ExpressionLength DescriptorString)
    ()
    Source
    Public
    Required
    .false.
    .false.
    .false.
    ()
    .false.
    .false.
)
```
***
```Fortran
  subroutine sub_(s1, s2)
    character(*) :: s1
    character(len(s1)) :: s2
  end subroutine
```
```Clojure
s1:
    (Variable
        3
        s1
        []
        Unspecified
        ()
        ()
        Default
        (String 1 () AssumedLength DescriptorString)
        ()
        Source
        Public
        Required
        .false.
        .false.
        .false.
        ()
        .false.
        .false.
    ),
s2:
    (Variable
        3
        s2
        [s1]
        Unspecified
        ()
        ()
        Default
        (String 1 (StringLen
            (Var 3 s1)
            (Integer 4)
            ()
        ) ExpressionLength DescriptorString)
        ()
        Source
        Public
        Required
        .false.
        .false.
        .false.
        ()
        .false.
        .false.
    )

```
*** 
```fortran
character(10),allocatable :: str(:) ! Deferred-size-array
```

```Clojure
str:
    (Variable
        2
        str
        []
        Local
        ()
        ()
        Default
        (Allocatable
            (Array
                (String 1 (IntegerConstant 10 (Integer 4) Decimal) ExpressionLength DescriptorString)
                [(()
                ())]
                DescriptorArray
            )
        )
        ()
        Source
        Public
        Required
        .false.
        .false.
        .false.
        ()
        .false.
        .false.
)
```

- **CChar**
    - Used for C interoperability only.
    - Corresponds to C's `char`


```Fortran
  subroutine foo(s) bind(c)
    use iso_c_binding , only : c_char
    character(kind=c_char) :: s
  end subroutine
```

```clojure
s:
    (Variable
        3
        s
        []
        Unspecified
        ()
        ()
        Default
        (String 1 (IntegerConstant 1 (Integer 4) Decimal) ExpressionLength CChar); Notice the phsyical type
        ()
        BindC
        Public
        Required
        .false.
        .false.
        .false.
        ()
        .false.
        .false.
    )
```
***
```fortran
  subroutine foo(s) bind(c)
    use iso_c_binding , only : c_char
    character(kind=c_char) :: s(10)
  end subroutine
```

```Clojure
s:
(Variable
    3
    s
    []
    Unspecified
    ()
    ()
    Default
    (Array
        (String 1 (IntegerConstant 1 (Integer 4) Decimal) ExpressionLength CChar)
        [((IntegerConstant 1 (Integer 4) Decimal)
        (IntegerConstant 10 (Integer 4) Decimal))]
        StringArraySinglePointer
    )
    ()
    BindC
    Public
    Required
    .false.
    .false.
    .false.
    ()
    .false.
    .false.
)
```

### See Also

[stringPhysicalCast](../expression_nodes/StringPhysicalCast.md)
 
