! Kernels in a module of another file (--separate-compilation), called
! from a module procedure of this file with a debug hook between them: the
! launches of a layer stay pending across the calls, and the host still
! sees what they wrote, in the hook when it runs and after the layers.
module gpu_deferred_sync_04_model
use, intrinsic :: iso_fortran_env, only: int16
use gpu_deferred_sync_04_mod
implicit none
type :: counts_t
    integer :: calls = 0
end type
contains
subroutine residual(x, delta, counts)
    integer(int16), contiguous, intent(inout) :: x(:,:)
    integer(int16), contiguous, intent(in) :: delta(:,:)
    type(counts_t), intent(inout) :: counts
    integer :: i
    do i = 1, size(x,2)
        call scale_add(x(:,i), delta(:,i))
    end do
    counts%calls = counts%calls + 1
end subroutine

subroutine forward(x, layers, directory, seen)
    integer(int16), contiguous, intent(inout) :: x(:,:)
    integer, intent(in) :: layers
    character(*), intent(in) :: directory
    integer, intent(out) :: seen
    integer(int16), allocatable :: y(:,:)
    type(counts_t) :: counts
    integer :: li
    seen = 0
    allocate(y(size(x,1), size(x,2)))
    do li = 1, layers
        call dm(x)
        call norm_like(x, y)
        call dm(y)
        call residual(x, y, counts)
        call dm(reshape(x, [size(x,1), size(x,2)]))
    end do
    if (counts%calls /= layers) error stop "counts"
contains
    subroutine dm(values)
        integer(int16), intent(in) :: values(:,:)
        if (len_trim(directory) == 0) return
        seen = seen + int(values(1,1))
    end subroutine
end subroutine
end module

program gpu_deferred_sync_04
use, intrinsic :: iso_fortran_env, only: int16
use gpu_deferred_sync_04_model
implicit none
integer(int16), allocatable :: x(:,:)
integer :: seen
allocate(x(64, 3))
x = 1_int16
call forward(x, 3, '', seen)
! x -> x + 2*(x + 1) = 3x + 2 each layer: 1 -> 5 -> 17 -> 53
if (any(x /= 53_int16)) error stop 1
if (seen /= 0) error stop 2
x = 1_int16
call forward(x, 2, 'dump', seen)
! layer 1: 1 + 2 + 5, layer 2: 5 + 6 + 17
if (seen /= 36) error stop 3
if (any(x /= 17_int16)) error stop 4
! An explicit-shape dummy, whose extent the module file's type names.
call add_explicit(size(x, 1), x(:, 2))
if (any(x(:, 2) /= 18_int16) .or. any(x(:, 1) /= 17_int16)) error stop 5
print *, x(1,1), seen
end program
