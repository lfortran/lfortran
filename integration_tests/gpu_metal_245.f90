! A BLOCK-local allocatable inside a `do concurrent` whose extent is not
! written in terms of the kernel arguments directly, but as `size(...)` of
! another array: of a kernel-argument array, of one dimension of one, or
! of a second BLOCK-local allocatable that is itself sized by the kernel
! arguments.  The workspace buffer for such an array is sized on the host
! at launch time, so every `size()` leaf has to be reproducible there.
module gpu_metal_245_mod
implicit none

contains

    ! Extent is size() of a BLOCK-local allocatable that itself has a
    ! resolved workspace extent, used twice and with a constant added.
    subroutine fill_local_size(a, nn, nr)
    real, intent(out) :: a(:,:)
    integer, intent(in) :: nn, nr
    integer :: jj
    do concurrent (jj = 1:nn)
        block
            real, allocatable :: t(:)
            real, allocatable :: w(:)
            integer :: p
            allocate(t(nr))
            do p = 1, nr
                t(p) = real(10*jj + p)
            end do
            allocate(w(size(t) + size(t) + 2))
            do p = 1, size(w)
                w(p) = t(1) + real(p)
            end do
            a(:, jj) = w
        end block
    end do
    end subroutine

    ! Extent is size() of a rank-1 allocatable kernel-argument array,
    ! whose extents the host passes to the kernel as scalars.
    subroutine fill_arg_size(a, v, nn)
    real, intent(out) :: a(:,:)
    real, allocatable, intent(in) :: v(:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        block
            real, allocatable :: w(:)
            integer :: p
            allocate(w(size(v)))
            do p = 1, size(v)
                w(p) = v(p) + real(100*jj)
            end do
            a(:, jj) = w
        end block
    end do
    end subroutine

    ! Extent is size(arg, dim) of one dimension of a rank-2 kernel
    ! argument, plus a scalar kernel argument.
    subroutine fill_arg_dim_size(a, m, nn, extra)
    real, intent(out) :: a(:,:)
    real, allocatable, intent(in) :: m(:,:)
    integer, intent(in) :: nn, extra
    integer :: jj
    do concurrent (jj = 1:nn)
        block
            real, allocatable :: w(:)
            integer :: p
            allocate(w(size(m, 2) + extra))
            do p = 1, size(m, 2) + extra
                w(p) = m(1, 1) + real(200*jj + p)
            end do
            a(:, jj) = w
        end block
    end do
    end subroutine

    ! Extent is the whole-array size() of a rank-2 allocatable kernel
    ! argument, i.e. the product of both of its extents.
    subroutine fill_whole_size(a, m, nn)
    real, intent(out) :: a(:,:)
    real, allocatable, intent(in) :: m(:,:)
    integer, intent(in) :: nn
    integer :: jj
    do concurrent (jj = 1:nn)
        block
            real, allocatable :: w(:)
            integer :: p
            allocate(w(size(m)))
            do p = 1, size(w)
                w(p) = m(1, 1) + real(300*jj + p)
            end do
            a(:, jj) = w
        end block
    end do
    end subroutine

end module

program gpu_metal_245
use gpu_metal_245_mod
implicit none
integer, parameter :: n = 6, nr = 3
real, allocatable :: v(:), m(:,:)
real :: a1(2*nr + 2, n), a2(4, n), a3(5 + 2, n), a4(2*5, n)
integer :: i, j

allocate(v(4), m(2, 5))
do i = 1, 4
    v(i) = real(i)
end do
m = 7.0

a1 = -1
call fill_local_size(a1, n, nr)
do j = 1, n
    do i = 1, 2*nr + 2
        if (abs(a1(i, j) - real(10*j + 1 + i)) > 1.0e-4) error stop "a1"
    end do
end do

a2 = -1
call fill_arg_size(a2, v, n)
do j = 1, n
    do i = 1, 4
        if (abs(a2(i, j) - real(i + 100*j)) > 1.0e-4) error stop "a2"
    end do
end do

a3 = -1
call fill_arg_dim_size(a3, m, n, 2)
do j = 1, n
    do i = 1, 7
        if (abs(a3(i, j) - (7.0 + real(200*j + i))) > 1.0e-4) error stop "a3"
    end do
end do

a4 = -1
call fill_whole_size(a4, m, n)
do j = 1, n
    do i = 1, 2*5
        if (abs(a4(i, j) - (7.0 + real(300*j + i))) > 1.0e-4) error stop "a4"
    end do
end do

print *, "ok"
end program
