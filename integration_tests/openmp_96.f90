! A finalizable function result referenced after an outlined OpenMP region is
! finalized once, after its statement.
!
! gfortran does not finalize this result; for it, no finalization is accepted.
module openmp_96_m
use iso_fortran_env, only: compiler_version
implicit none
integer :: nfin = 0, ncalls = 0
type :: h
    integer, pointer :: p => null()
contains
    final :: fin_h
end type
contains
function mk(c) result(r)
    integer, intent(in) :: c
    type(h) :: r
    ncalls = ncalls + 1
    allocate(r%p)
    r%p = c
end function

integer function pv(s)
    type(h), intent(in) :: s
    if (.not. associated(s%p)) error stop "pv: result already finalized"
    pv = s%p
end function

subroutine fin_h(self)
    type(h), intent(inout) :: self
    nfin = nfin + 1
    if (associated(self%p)) deallocate(self%p)
end subroutine
end module

program openmp_96
use openmp_96_m
implicit none
integer :: i, k, b(8)
logical :: strict
strict = index(compiler_version(), "GCC") == 0
!$omp parallel do
do i = 1, 8
    b(i) = 2*i
end do
!$omp end parallel do
k = pv(mk(b(3)))
if (k /= 6) error stop "value"
if (ncalls /= 1) error stop "calls"
if (nfin /= 1 .and. (strict .or. nfin /= 0)) error stop "finalizations"
nfin = 0; ncalls = 0
do i = 1, 8
    if (pv(mk(b(i))) > 100) exit
end do
if (ncalls /= 8) error stop "loop calls"
if (nfin /= 8 .and. (strict .or. nfin /= 0)) error stop "loop finalizations"
print *, k
end program
