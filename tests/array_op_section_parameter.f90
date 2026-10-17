! The value of a named constant `c` of a module is substituted for it. Like
! a literal value, it is named as a local named constant, so the section
! target `a(:, i)` is assigned through its base array `a` element by element
! from that constant, without a pointer to the section.
module array_op_section_parameter_m
implicit none
real, parameter :: c(4) = [1., 2., 3., 4.]
contains
subroutine s(a, i)
real, intent(inout) :: a(4, 3)
integer, intent(in) :: i
a(:, i) = c
end subroutine
end module
