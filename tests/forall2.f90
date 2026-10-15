module forall2_mod
contains
pure real function f(x)
real, intent(in) :: x
f = 2 * x
end function
end module

subroutine forall2(a, b, c)
use forall2_mod
real, intent(inout) :: a(:), c(:,:)
real, intent(in) :: b(:)
integer :: i, j
! Single DO CONCURRENT loops: no iteration reads what another one assigns.
forall (i = 1:size(a)) a(i) = a(i) * 0.5 + 1
forall (i = 1:size(a), j = 1:size(a)) c(i,j) = c(i,j) + a(i)
forall (i = 1:size(a)) a(i) = f(b(i))
! Temporaries: a(i) reads a(size(a) + 1 - i).
forall (i = 1:size(a)) a(i) = a(size(a) + 1 - i)
end subroutine
