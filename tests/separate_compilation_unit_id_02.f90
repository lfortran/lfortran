! The name of the compiler-generated function that implements `modulo` here
! is the same in every compilation.
subroutine separate_compilation_unit_id_02(i, j, k)
implicit none
integer, intent(in) :: i, j
integer, intent(out) :: k
k = modulo(i, j)
end subroutine
