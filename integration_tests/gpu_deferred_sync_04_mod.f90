module gpu_deferred_sync_04_mod
use, intrinsic :: iso_fortran_env, only: int16
implicit none
private
public :: norm_like, scale_add, add_explicit
contains
pure subroutine require_same(x, y)
    integer(int16), intent(in) :: x(:,:), y(:,:)
    if (any(shape(x) /= shape(y))) error stop "shape mismatch"
end subroutine

pure subroutine norm_like(x, y)
    integer(int16), contiguous, intent(in) :: x(:,:)
    integer(int16), contiguous, intent(out) :: y(:,:)
    integer :: m, k
    call require_same(x, y)
    do concurrent (m = 1:size(x,2), k = 1:size(x,1))
        y(k,m) = x(k,m) + 1_int16
    end do
end subroutine

pure subroutine scale_add(x, delta)
    integer(int16), contiguous, intent(inout) :: x(:)
    integer(int16), contiguous, intent(in) :: delta(:)
    integer :: i
    if (size(x) /= size(delta)) error stop "size mismatch"
    do concurrent (i = 1:size(x))
        x(i) = x(i) + 2_int16*delta(i)
    end do
end subroutine

subroutine add_explicit(n, x)
    integer, intent(in) :: n
    integer(int16), intent(inout) :: x(n)
    integer :: i
    do concurrent (i = 1:n)
        x(i) = x(i) + 1_int16
    end do
end subroutine
end module
