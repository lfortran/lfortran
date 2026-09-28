! A function with a finalizable result referenced in a bound of an OpenMP
! worksharing loop or of a DO CONCURRENT loop that the OpenMP pass outlines.
! Each evaluation of the bound is finalized once, after the result is used.
! One thread runs the loops, so that the counters are not updated
! concurrently.
!
! gfortran does not finalize these results; for it, no finalization is
! accepted.
module openmp_97_m
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
    if (associated(self%p)) then
        nfin = nfin + 1
        deallocate(self%p)
    end if
end subroutine
end module

program openmp_97
use omp_lib, only: omp_set_num_threads
use openmp_97_m
implicit none
integer :: i, a(100)
logical :: strict
strict = index(compiler_version(), "GCC") == 0
call omp_set_num_threads(1)

a = 0
!$omp parallel
!$omp do
do i = 1, pv(mk(42)) / 2
    a(i) = 2*i
end do
!$omp end do
!$omp end parallel
if (sum(a) /= 462) error stop "omp do: sum"
if (ncalls /= 1) error stop "omp do: calls"
if (nfin /= 1 .and. (strict .or. nfin /= 0)) error stop "omp do: finalizations"

nfin = 0; ncalls = 0
a = 0
do concurrent (i = 1:pv(mk(42)) / 2)
    a(i) = i
end do
if (sum(a) /= 231) error stop "do concurrent: sum"
if (ncalls /= 1) error stop "do concurrent: calls"
if (nfin /= 1 .and. (strict .or. nfin /= 0)) error stop "do concurrent: finalizations"
print *, "ok"
end program
