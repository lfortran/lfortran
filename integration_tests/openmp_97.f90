! A function with a finalizable result referenced in a bound of an OpenMP
! worksharing loop, of a distribute loop or of a DO CONCURRENT loop that the
! OpenMP pass outlines. Each evaluation of the bound is finalized once, after
! the loop has been executed. One thread (and one team) runs the loops whose
! counters are checked, so that the counters are not updated concurrently.
! The other loops run on several threads or teams, with a function that
! updates no counter, and check only the results.
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
type :: q
    integer, pointer :: p => null()
contains
    final :: fin_q
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

! As mk, but the result updates no counter, so that several threads may
! reference it at the same time.
function mk_quiet(c) result(r)
    integer, intent(in) :: c
    type(q) :: r
    allocate(r%p)
    r%p = c
end function

integer function qv(s)
    type(q), intent(in) :: s
    if (.not. associated(s%p)) error stop "qv: result already finalized"
    qv = s%p
end function

subroutine fin_q(self)
    type(q), intent(inout) :: self
    if (associated(self%p)) deallocate(self%p)
end subroutine

! The number of finalizations so far, for the loop bodies. The OpenMP pass
! does not give a loop body it outlines access to a variable of the module.
pure integer function finalized()
    finalized = nfin
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
integer :: i, j, s, t, a(100), b(10, 10), seen(100)
logical :: strict
strict = index(compiler_version(), "GCC") == 0
call omp_set_num_threads(1)

seen = -1
a = 0
!$omp parallel
!$omp do
do i = 1, pv(mk(42)) / 2
    a(i) = 2*i
    seen(i) = finalized()
end do
!$omp end do
!$omp end parallel
if (sum(a) /= 462) error stop "omp do: sum"
if (ncalls /= 1) error stop "omp do: calls"
if (any(seen(1:21) /= 0)) error stop "omp do: finalized in the loop"
if (nfin /= 1 .and. (strict .or. nfin /= 0)) error stop "omp do: finalizations"

nfin = 0; ncalls = 0; seen = -1
a = 0
do concurrent (i = 1:pv(mk(42)) / 2)
    a(i) = i
    seen(i) = finalized()
end do
if (sum(a) /= 231) error stop "do concurrent: sum"
if (ncalls /= 1) error stop "do concurrent: calls"
if (any(seen(1:21) /= 0)) error stop "do concurrent: finalized in the loop"
if (nfin /= 1 .and. (strict .or. nfin /= 0)) error stop "do concurrent: finalizations"

call omp_set_num_threads(4)

s = 0
!$omp parallel do reduction(+:s) schedule(dynamic, 3)
do i = 1, qv(mk_quiet(42)) / 2
    s = s + i
end do
!$omp end parallel do
if (s /= 231) error stop "4 threads, parallel do: sum"

b = 0
!$omp parallel do collapse(2)
do i = 1, qv(mk_quiet(10))
    do j = qv(mk_quiet(2)), qv(mk_quiet(10))
        b(i, j) = i*j
    end do
end do
!$omp end parallel do
if (sum(b) /= 55*54) error stop "4 threads, collapse: sum"

nfin = 0; ncalls = 0; seen = -1
t = 0
!$omp teams distribute num_teams(1) reduction(+:t)
do i = 1, pv(mk(42)) / 2
    t = t + i
    seen(i) = finalized()
end do
!$omp end teams distribute
if (t /= 231) error stop "teams distribute: sum"
if (ncalls /= 1) error stop "teams distribute: calls"
if (any(seen(1:21) /= 0)) error stop "teams distribute: finalized in the loop"
if (nfin /= 1 .and. (strict .or. nfin /= 0)) error stop "teams distribute: finalizations"

nfin = 0; ncalls = 0; seen = -1
a = 0
!$omp teams num_teams(1)
!$omp distribute
do i = 1, pv(mk(42)) / 2
    a(i) = i
    seen(i) = finalized()
end do
!$omp end distribute
!$omp end teams
if (sum(a) /= 231) error stop "distribute: sum"
if (ncalls /= 1) error stop "distribute: calls"
if (any(seen(1:21) /= 0)) error stop "distribute: finalized in the loop"
if (nfin /= 1 .and. (strict .or. nfin /= 0)) error stop "distribute: finalizations"

nfin = 0; ncalls = 0; seen = -1
a = 0
!$omp teams num_teams(1) thread_limit(1)
!$omp distribute parallel do
do i = 1, pv(mk(42)) / 2
    a(i) = 3*i
    seen(i) = finalized()
end do
!$omp end distribute parallel do
!$omp end teams
if (sum(a) /= 693) error stop "distribute parallel do: sum"
if (ncalls /= 1) error stop "distribute parallel do: calls"
if (any(seen(1:21) /= 0)) error stop "distribute parallel do: finalized in the loop"
if (nfin /= 1 .and. (strict .or. nfin /= 0)) error stop "distribute parallel do: finalizations"

t = 0
!$omp teams distribute num_teams(4) reduction(+:t)
do i = 1, qv(mk_quiet(42)) / 2
    t = t + i
end do
!$omp end teams distribute
if (t /= 231) error stop "4 teams, teams distribute: sum"

a = 0
!$omp teams num_teams(2)
!$omp distribute parallel do
do i = 1, qv(mk_quiet(42)) / 2
    a(i) = i
end do
!$omp end distribute parallel do
!$omp end teams
if (sum(a) /= 231) error stop "2 teams, distribute parallel do: sum"

print *, "ok"
end program
