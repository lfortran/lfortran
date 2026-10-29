# Array and structure temporaries

The default `pass_array_struct_temporary` entry point preserves section views
where an alias is sufficient. The C++ preparation pipeline instead selects
`ArraySectionLowering::ValueSnapshot` for numeric and logical section values
of a named, nonpointer array, with scalar/triplet selectors of integer kind at
most four. Vector subscripts, SIMD values and WHERE retain their existing
handling; this policy does not implement pointer association or a new array ABI.
Procedure-actual association and copy-out keep their existing handling.

Both expression-operand preparation and assignment-value replacement use the
shared owning-temporary helper. Mutable scalar subscripts and triplet controls
are captured before sizing and copying. The temporary has the section's element
type and rank, lower bounds of one, and nonnegative extents computed from the
source triplets, independently of the destination.

The temporary is filled through a full-section target: its storage has already
been allocated, so automatic-reallocation modes must not resize it while
gathering. Existing `array_op` lowering turns the fill and final assignment into
scalar accesses and loops. The complete snapshot precedes destination writes,
including overlapping/reversed assignments. C++ ASR verification and readiness
checks run before emission; the backend does not implement section traversal.
