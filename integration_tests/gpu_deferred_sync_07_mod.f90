module gpu_deferred_sync_07_mod
implicit none
contains
subroutine scaled(x, y)
    real, intent(in) :: x(:)
    real, intent(out) :: y(:)
    integer :: i
    do concurrent (i = 1:size(x))
        if (x(i) < 0) error stop "scaled requires nonnegative values"
        y(i) = 2*x(i)
    end do
end subroutine
end module
