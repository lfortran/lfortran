# String Physical Cast
- Cast string physical type from one physicalType to another (E.g. [`DescriptorString`](../type_nodes/StringPhysicalType.md) to [`CChar`](../type_nodes/StringPhysicalType.md) and vice versa).

- It's responsible of setting the string expression to the right physical type to make facilitate the work at the back end.

- It's only used with passed arguments in function calls.
## Declaration

### Syntax

```ASDL
StringPhysicalCast(expr arg, 
	string_physical_type old, 
	string_physical_type new, 
	ttype type,
	expr? value)
```

### Arguments

| Argument Name | Argument Description                                                                                                        |
| ------------- | --------------------------------------------------------------------------------------------------------------------------- |
| `arg`         | String expression argument                                                                                                  |
| `old`         | Current physical string type of the string expression `arg`. <br>**Possible values** : `CChar`, `DescriptorString`. |
| `new`         | The target physical string type to cast to.<br>**Possible values** : `DescriptorString`, `CChar`.                   |
| `type`        | Always a String type with `length_kind` = `ImplicitLength`                         |
| `value`       |                          |

### Return value

The return value is a an expression corresponding to `arg` but with physicalType corresponding to `new`.

## Description

**StringPhysicalCast** casts the physical type of string-type expressions to facilitate the work done on the back end.

 It dispatches a message to the backend to cast the physical type of the passed string expression from one type to another, In order to make the work in the back end much easier, otherwise the back end will complain.

**The intention behind casting from [`DescriptorString`](../type_nodes/StringPhysicalType.md) to [`CChar`](../type_nodes/StringPhysicalType.md) :**
- The intention is to change the physical type from descriptorString which is an llvm struct`{char* data, int64 size, int64 capacity}` to C's char type (`char`).
- It's a necessity when we send an argument of physical type `DescriptorString` to a function that accepts a `CChar` (A bindC function). 
- This happens by fetching string's data member from the LLVM descriptor. 

**The intention behind casting from [`CChar`](../type_nodes/StringPhysicalType.md) to [`DescriptorString`](../type_nodes/StringPhysicalType.md):**\
- The intention is to change the physical type from `CChar` which is an llvm struct (`char*`) to DescriptorString which is an llvm-struct (`{char* data, int64 size, int64 capacity}`).
- This happens by creating an LLVM struct (which represents the descriptorString) called `stringCast_desc` then we just set its internal `char*` to the casted from `arg` + We set its length to the compile-time length that user defined (atleast for now).


## Types

Only accepts string type.
## Examples

```fortran
program str_physical_cast
  character(10) :: str ! PhysicalType = DescriptorString
  
  call foo(str)

  contains
  subroutine foo(s) bind(c)
    use iso_c_binding , only : c_char
    character(kind=c_char) :: s ! PhysicalType = CChar
  end subroutine  
end program

```

ASR:

```Clojure
(SubroutineCall
    2 foo
    ()
    [((StringPhysicalCast
        (Var 2 str)
        DescriptorString
        CChar
        (String 1 () ImplicitLength CChar)
        ()
    ))]
    ()
)
```

(Backend) LLVM-11 :

```llvm
%10 = getelementptr %string_descriptor, %string_descriptor* %str, i32 0, i32 0
%11 = load i8*, i8** %10, align 8
call void @foo(i8* %11)
```

## See Also

[`DescriptorString`](../type_nodes/../type_nodes/StringPhysicalType.md)