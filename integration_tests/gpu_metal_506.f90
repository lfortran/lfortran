! Arrays a kernel assigns whole before it reads them, as a solver step has
! them: allocatable components of a derived type, a local work array, faces
! one longer than the cells in one dimension, sections handed to a
! procedure, and lower bounds other than one. What the kernels do not assign
! must keep its value, and every result must be the one the host would get.
module gpu_metal_506_m
implicit none

type fields_t
    real, allocatable :: base(:,:), q(:,:), acc(:,:), fx(:,:), fy(:,:)
end type

contains

    subroutine step(f, nx, ny, steps)
        type(fields_t), intent(inout) :: f
        integer, intent(in) :: nx, ny, steps
        real, allocatable :: w(:,:)
        integer :: s, i, j

        allocate(w(1:nx + 1, 1:ny))
        do concurrent (j = 1:ny, i = 1:nx + 1)
            w(i, j) = 0.5 * real(i + j)
        end do
        do concurrent (j = 1:ny, i = 1:nx)
            f%q(i, j) = f%base(i, j)
            f%acc(i, j) = 0
        end do
        do s = 1, steps
            do concurrent (j = 1:ny + 1, i = 1:nx + 1)
                if (i <= nx + 1 .and. j <= ny) then
                    f%fx(i, j) = w(i, j) * f%q(min(i, nx), j)
                end if
                if (j >= 1 .and. i < nx + 1) then
                    f%fy(i, j) = f%q(i, min(j, ny)) - 1
                end if
            end do
            do concurrent (j = 1:ny, i = 1:nx)
                f%acc(i, j) = f%acc(i, j) + f%fx(i + 1, j) - f%fx(i, j) &
                    + f%fy(i, j + 1) - f%fy(i, j)
                f%q(i, j) = f%base(i, j) + 0.25 * f%acc(i, j)
            end do
        end do
        deallocate(w)
    end subroutine

    subroutine fill(a, v)
        real, intent(inout) :: a(:,:)
        real, intent(in) :: v
        integer :: i, j
        do concurrent (j = 1:size(a, 2), i = 1:size(a, 1))
            a(i, j) = v + real(i) + 10 * real(j)
        end do
    end subroutine

    subroutine fill_from(z, lo, hi)
        integer, intent(in) :: lo, hi
        real, intent(inout) :: z(0:)
        integer :: i
        do concurrent (i = lo:hi)
            z(i) = real(i)
        end do
    end subroutine

    ! The step on the host, to check the device against.
    subroutine step_host(f, nx, ny, steps)
        type(fields_t), intent(inout) :: f
        integer, intent(in) :: nx, ny, steps
        real :: w(nx + 1, ny)
        integer :: s, i, j
        do j = 1, ny
            do i = 1, nx + 1
                w(i, j) = 0.5 * real(i + j)
            end do
        end do
        f%q = f%base
        f%acc = 0
        do s = 1, steps
            do j = 1, ny + 1
                do i = 1, nx + 1
                    if (j <= ny) f%fx(i, j) = w(i, j) * f%q(min(i, nx), j)
                    if (i < nx + 1) f%fy(i, j) = f%q(i, min(j, ny)) - 1
                end do
            end do
            do j = 1, ny
                do i = 1, nx
                    f%acc(i, j) = f%acc(i, j) + f%fx(i + 1, j) - f%fx(i, j) &
                        + f%fy(i, j + 1) - f%fy(i, j)
                    f%q(i, j) = f%base(i, j) + 0.25 * f%acc(i, j)
                end do
            end do
        end do
    end subroutine

    subroutine init(f, nx, ny)
        type(fields_t), intent(out) :: f
        integer, intent(in) :: nx, ny
        integer :: i, j
        allocate(f%base(nx, ny), f%q(nx, ny), f%acc(nx, ny))
        allocate(f%fx(nx + 1, ny), f%fy(nx, ny + 1))
        do j = 1, ny
            do i = 1, nx
                f%base(i, j) = real(mod(3 * i + j, 7))
            end do
        end do
        f%q = 99
        f%acc = 99
        f%fx = 99
        f%fy = 99
    end subroutine

end module

program gpu_metal_506
use gpu_metal_506_m
implicit none
integer, parameter :: nx = 7, ny = 5
type(fields_t) :: f, g
real :: a(6, 4), z(0:5)
integer :: k, i, j

do k = 1, 3
    call init(f, nx, ny)
    call init(g, nx, ny)
    call step(f, nx, ny, k)
    call step_host(g, nx, ny, k)
    if (.not. all(abs(f%q - g%q) <= 1e-4)) error stop 1
    if (.not. all(abs(f%acc - g%acc) <= 1e-4)) error stop 2
    if (.not. all(abs(f%fx - g%fx) <= 1e-4)) error stop 3
    if (.not. all(abs(f%fy - g%fy) <= 1e-4)) error stop 4
end do

! A contiguous section: only its columns change.
a = -1
call fill(a(:, 2:3), 100.0)
do j = 1, 4
    do i = 1, 6
        if (j == 2 .or. j == 3) then
            if (a(i, j) /= 100 + i + 10 * (j - 1)) error stop 5
        else
            if (a(i, j) /= -1) error stop 6
        end if
    end do
end do

! A strided section: only its rows change.
a = -1
call fill(a(1:6:2, :), 200.0)
do j = 1, 4
    do i = 1, 6
        if (mod(i, 2) == 1) then
            if (a(i, j) /= 200 + (i + 1) / 2 + 10 * j) error stop 7
        else
            if (a(i, j) /= -1) error stop 8
        end if
    end do
end do

! A lower bound of zero: the loop from one leaves the first element.
z = -1
call fill_from(z, 1, 5)
if (z(0) /= -1) error stop 9
if (any(z(1:5) /= [1., 2., 3., 4., 5.])) error stop 10
z = -1
call fill_from(z, 0, 5)
if (any(z /= [0., 1., 2., 3., 4., 5.])) error stop 11
print *, "ok"
end program
