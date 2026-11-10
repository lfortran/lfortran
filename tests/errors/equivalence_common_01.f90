! Two scalar variables of the same common block cannot be equivalenced: the
! common block already fixes their storage (#12524).
subroutine s
real :: a, b
common /c1/ a, b
equivalence (a, b)  ! {Error} equivalence between two common block variables is not allowed
end subroutine
