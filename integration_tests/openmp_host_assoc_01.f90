! A procedure with an internal procedure that uses its variables is called by
! several threads at once (#16012)
module openmp_host_assoc_01_m
implicit none
contains
subroutine host(x, r)
    integer, intent(in) :: x
    integer, intent(out) :: r
    integer :: k
    r = 0
    do k = 1, 100
        call inner()
    end do
contains
    subroutine inner()
        r = r + x
    end subroutine
end subroutine

integer function host_fn(x) result(s)
    integer, intent(in) :: x
    integer :: k, m
    m = x
    s = 0
    do k = 1, 200
        s = s + inner()
    end do
contains
    integer function inner()
        inner = ident(m)
    end function
end function

integer function ident(i)
    integer, intent(in) :: i
    ident = i
end function
end module

program openmp_host_assoc_01
use omp_lib
use openmp_host_assoc_01_m
implicit none
integer :: i, a(1000), b(2000), bad
call omp_set_num_threads(4)
!$omp parallel do
do i = 1, 1000
    call host(i, a(i))
end do
!$omp end parallel do
!$omp parallel do
do i = 1, 2000
    b(i) = host_fn(i)
end do
!$omp end parallel do
bad = 0
do i = 1, 1000
    if (a(i) /= 100*i) bad = bad + 1
end do
do i = 1, 2000
    if (b(i) /= 200*i) bad = bad + 1
end do
print *, bad
if (bad /= 0) error stop 1
end program
