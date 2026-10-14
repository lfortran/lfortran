module openmp_104_mod
implicit none
integer, parameter :: mp(3) = [10, 20, 30]
integer, parameter :: c_int = 11
end module openmp_104_mod

program openmp_104
! Named constant arrays and array constructors used in OpenMP regions
use openmp_104_mod, only: mp
implicit none
integer, parameter :: n = 8
real, parameter :: c(3) = [1.0, 2.0, 3.0]
real(8), parameter :: z(2) = [1.5d0, 2.5d0]
real :: a(3, n), b(2)
real(8) :: r(2, n)
integer :: k(3, n)
integer :: i

a = 0
r = 0
k = 0
!$omp parallel do
do i = 1, n
    a(:, i) = c
    r(:, i) = z
    k(:, i) = mp
end do
!$omp end parallel do
if (any(a(2, :) /= 2.0)) error stop 1
if (any(r(2, :) /= 2.5d0)) error stop 2
if (any(k(3, :) /= 30)) error stop 3

a = 0
!$omp parallel do
do i = 1, n
    a(:, i) = [4.0, 5.0, 6.0]
end do
!$omp end parallel do
if (any(a(2, :) /= 5.0)) error stop 4

a = 0
!$omp parallel
!$omp do
do i = 1, n
    a(:, i) = [7.0, 8.0, 9.0]
end do
!$omp end do
!$omp end parallel
if (any(a(3, :) /= 9.0)) error stop 5

b = 0
!$omp parallel
!$omp single
b = [7.0, 8.0]
!$omp end single
!$omp end parallel
if (b(1) /= 7.0 .or. b(2) /= 8.0) error stop 6

b = 0
!$omp parallel
!$omp single
!$omp task
b = [3.0, 4.0]
!$omp end task
!$omp end single
!$omp end parallel
if (b(1) /= 3.0 .or. b(2) /= 4.0) error stop 7

a = 0
do concurrent (i = 1:n)
    a(:, i) = [1.0, 2.0, 3.0]
end do
if (any(a(2, :) /= 2.0)) error stop 8

call host_associated()
call name_clashes()
print *, "ok"

contains

    subroutine host_associated()
    integer :: m(3, n), j
    m = 0
    !$omp parallel do
    do j = 1, n
        m(:, j) = mp + int(c)
    end do
    !$omp end parallel do
    if (any(m(1, :) /= 11)) error stop 9
    end subroutine host_associated

    ! Named constants with the names of entities that the procedures of the
    ! regions declare themselves (iso_c_binding, the region's data)
    subroutine name_clashes()
    use openmp_104_mod, only: c_int
    integer, parameter :: c_bool = 7, c_long(2) = [3, 4], data = 5, tdata = 6
    integer :: m(n), j
    m = 0
    !$omp parallel do
    do j = 1, n
        m(j) = c_bool * j + c_int + sum(c_long) + data + tdata
    end do
    !$omp end parallel do
    do j = 1, n
        if (m(j) /= 7 * j + 29) error stop 10
    end do

    m = 0
    !$omp parallel
    !$omp parallel do
    do j = 1, n
        m(j) = c_bool + data
    end do
    !$omp end parallel do
    !$omp end parallel
    if (any(m(1:n) /= 12)) error stop 11

    m = 0
    !$omp parallel
    !$omp single
    !$omp task
    m(1) = c_long(1) + tdata + c_int
    !$omp end task
    !$omp end single
    !$omp end parallel
    if (m(1) /= 20) error stop 12

    m = 0
    !$omp teams num_teams(2)
    !$omp distribute parallel do
    do j = 1, n
        m(j) = c_bool * j + data
    end do
    !$omp end distribute parallel do
    !$omp end teams
    do j = 1, n
        if (m(j) /= 7 * j + 5) error stop 13
    end do
    end subroutine name_clashes

end program openmp_104
