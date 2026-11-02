# TraitArrayAllocate

Allocate the elements of a runtime trait array owner (TD-46).

## Declaration

### Syntax

```text
TraitArrayAllocate(expr target, dimension* dims, expr? source, symbol witness)
```

`target` is a variable of a deferred-shape allocatable runtime trait array
type, `Allocatable(Array(TraitObjectType))`, and `dims` gives the lower bound
(1 when absent) and the extent of each of its dimensions. `witness` is the
witness of the instantiable concrete type of every element, conforming to the
target's exact contract. Without a `source` the allocation was typed and
default-initializes each element; with one, `source` is a nonpolymorphic
scalar of that concrete type, which initialize-copies each element, as
`allocate(arr(n), source=x)` does.

The operation requires an unallocated target. It allocates contiguous storage
for the elements, records their size and byte strides in the descriptor,
stores the witness's view without a payload after the dimensions, and
initializes each element through the witness's lifecycle. Deallocating the
owner destroys each element through the same lifecycle before freeing the
storage. Verification checks the target, the bounds, the witness's contract
and concrete type, and the source's type.
