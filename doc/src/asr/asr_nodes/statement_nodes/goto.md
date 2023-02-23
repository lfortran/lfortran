# GoTo

Branches to a statement label, a statement node.

## Declaration

### Syntax

```fortran
GoTo(int target_id, identifier name)
```

### Arguments

`target_id` contains `int` IDs to link `GoTo` with GoToTarget.
`name` contains the identifier name.

### Return values

None.

## Description

**GoTo** statement points to a GoToTarget with the corresponding `target_id`
within the same procedure. We currently use `int` IDs to link GoTo with
GoToTarget to avoid issues with serialization.

## Types

Integer or `target_id` and a valid identifier name.

## Examples

```fortran
program goto
implicit none
integer :: a
a = 5
go to 1
1 print *, a
end program
```

ASR:

```fortran
(TranslationUnit
    [(Program
        goto_01
        ()
        []
        [(ImplicitNone
            []
            ()
        )]
        [(Declaration
            (AttrType
                TypeInteger
                []
                ()
                None
            )
            []
            [(a
            []
            []
            ()
            None
            ())]
            ()
        )]
        [(=
            0
            a
            5
            ()
        )
        (GoTo
            0
            ()
            1
            []
            ()
        )
        (Print
            1
            ()
            [a]
            ()
        )]
        []
    )]
)
```

## See Also

[GoToTarget](gototarget.md)
