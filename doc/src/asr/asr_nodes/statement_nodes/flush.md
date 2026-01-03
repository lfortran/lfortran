# Flush

Flushes Fortran unit currently open for output, a `stmt` node.


## Declaration

### Syntax

```fortran
Flush(int label, expr unit, expr? err, expr? iomsg, expr? iostat)
```

### Arguments

`label` contains label of the branch target statement that receives control if
an error occurs.

`unit` contains external unit specifier.

`err` contains error message.

`iomsg` contains an explanatory message if an I/O error occurs.

`iostat` contains return value of the operation, the status code.

### Return values

None.

## Description

**Flush** statement makes the data written to a file or i/o unit to become
available to other processes or causes data written to a file outside
of Fortran to be accessible to a READ statement.

## Types


## Examples

```fortran
program rewind_inquire_flush
    implicit none
    integer :: ios, len, a, b
    character :: fm
    logical :: ext
    rewind(unit=9, iostat=ios, err=10)
    inquire (file='file_b', exist=ext)
    inquire (4, form=fm, iostat=ios, err=20)
    inquire (iolength=len) a, b
    10 print *, "err rewind"
    20 print *, "err inquire"
end program
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            rewind_inquire_flush:
                (Program
                    (SymbolTable
                        2
                        {
                            a:
                                (Variable
                                    2
                                    a
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Integer 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            b:
                                (Variable
                                    2
                                    b
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Integer 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            ext:
                                (Variable
                                    2
                                    ext
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Logical 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            fm:
                                (Variable
                                    2
                                    fm
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Character 1 1 () [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            ios:
                                (Variable
                                    2
                                    ios
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Integer 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                ),
                            len:
                                (Variable
                                    2
                                    len
                                    Local
                                    ()
                                    ()
                                    Default
                                    (Integer 4 [])
                                    Source
                                    Public
                                    Required
                                    .false.
                                )

                        })
                    rewind_inquire_flush
                    []
                    [(FileRewind
                        0
                        (IntegerConstant 9 (Integer 4 []))
                        (Var 2 ios)
                        (IntegerConstant 10 (Integer 4 []))
                    )
                    (FileInquire
                        0
                        ()
                        (StringConstant
                            "file_b"
                            (Character 1 6 () [])
                        )
                        ()
                        ()
                        (Var 2 ext)
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                    )
                    (FileInquire
                        0
                        (IntegerConstant 4 (Integer 4 []))
                        ()
                        (Var 2 ios)
                        (IntegerConstant 20 (Integer 4 []))
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        (Var 2 fm)
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                    )
                    (FileInquire
                        0
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        ()
                        (Var 2 len)
                    )
                    (GoToTarget
                        10
                        10
                    )
                    (Print
                        ()
                        [(StringConstant
                            "err rewind"
                            (Character 1 10 () [])
                        )]
                        ()
                        ()
                    )
                    (GoToTarget
                        20
                        20
                    )
                    (Print
                        ()
                        [(StringConstant
                            "err inquire"
                            (Character 1 11 () [])
                        )]
                        ()
                        ()
                    )]
                )

        })
    []
)

```

## See Also

[Nullify](nullify.md)
