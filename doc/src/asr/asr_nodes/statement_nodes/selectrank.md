# SelectRank

Select Rank statement, a `stmt` node.

## Declaration

### Syntax

```fortran
SelectRank(identifier? name, expr selector, rank_stmt* body, stmt* default)
```

### Arguments

`name` is an optional construct name for the select rank construct. When
present, it allows `exit` statements to reference this construct by name
(e.g., `exit my_rank`). This corresponds to the Fortran named construct
syntax `my_rank: select rank(x)`.

`selector` contains the expression whose rank is tested.

`body` contains 0 or more rank statement branches.

`default` contains 0 or more statements executed when no rank branch matches
(corresponds to `rank default`).

### Return values

None.

## Description

**SelectRank** executes different blocks of code depending on the rank of
an assumed-rank array argument. The selector must be an assumed-rank
(`dimension(..)`) dummy argument.

Each `rank_stmt` branch specifies either a particular integer rank
(`rank(N)`) or the unranked case (`rank(*)`). If the actual rank matches,
the corresponding block is executed. If no branch matches and a `default`
block is present, that block is executed instead.

Named select rank constructs (using the `name` field) allow an `exit`
statement inside the body to transfer control out of the construct, e.g.:

```fortran
rank_loop: select rank(x)
rank(1)
    if (some_condition) exit rank_loop
rank(2)
    ...
end select rank_loop
```

## Types

Optional identifier, expression, and pointers to rank statement branches
and default body.

## Examples

```fortran
subroutine foo(input)
    class(*), dimension(..), intent(in) :: input
    rank_select: select rank(input)
    rank(1)
        select type(input)
        type is(real)
            exit rank_select
        end select
    end select rank_select
end subroutine
```

## See Also

[Select](select.md)
