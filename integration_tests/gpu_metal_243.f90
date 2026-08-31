! A BLOCK-local allocatable inside a `do concurrent` whose extent comes
! from a derived-type component rather than from a plain scalar kernel
! argument. The workspace buffer is sized on the host at launch time, so
! the extent expression has to be reproduced there: the component chain is
! loaded out of the struct that is passed to the kernel as a buffer.
module gpu_metal_243_mod
implicit none
type :: sizes_t
    integer :: rows
    integer :: extra
end type
end module

program gpu_metal_243
use gpu_metal_243_mod
implicit none
integer, parameter :: n = 6
type(sizes_t) :: s
real :: a(3, n), b(4, n)
integer :: i, j

s%rows = 3
s%extra = 1
a = 0.0
b = 0.0

call run_kernel(s, a, b, n)

do j = 1, n
    do i = 1, 3
        if (abs(a(i, j) - real(50*j + i)) > 1.0e-4) error stop "a"
    end do
    do i = 1, 4
        if (abs(b(i, j) - real(90*j + i)) > 1.0e-4) error stop "b"
    end do
end do

print *, "ok"

contains

    subroutine run_kernel(s, a, b, nn)
    type(sizes_t), intent(in) :: s
    real, intent(out) :: a(:,:), b(:,:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        block
            ! Extent is a struct component.
            real, allocatable :: t(:)
            ! Extent is an arithmetic expression over two components.
            real, allocatable :: w(:)
            integer :: p
            allocate(t(s%rows))
            allocate(w(s%rows + s%extra))
            do p = 1, s%rows
                t(p) = real(50*jj + p)
            end do
            do p = 1, s%rows + s%extra
                w(p) = real(90*jj + p)
            end do
            a(:, jj) = t
            b(:, jj) = w
        end block
    end do
    end subroutine

end program
