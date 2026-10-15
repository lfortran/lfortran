! A section target is assigned through its base array `a`. The extent of
! `a(:, i)` is a compile-time 4, equal to that of `b`, so the first
! assignment needs no shape check. The scalar subscript `i` is checked once,
! by referencing the first element of the section before the loop, and the
! loop references the elements of `a` and `b` without bounds checks. In
! `a(2:k, i)` the end `k` of the triplet can exceed the bounds of `a`, so the
! loop references `a` with bounds checks. The extent of `a(2:4, i)` differs
! from that of `b`, which a runtime shape check reports.
subroutine array_op_section_in_bounds(a, b, i, k)
implicit none
real, intent(inout) :: a(4, 3)
real, intent(in) :: b(4)
integer, intent(in) :: i, k
a(:, i) = b
a(2:k, i) = 1.
a(2:4, i) = b
end subroutine
