module arrays_op_31_mod
    implicit none
contains

    module function softmax_r3_sp(x, dim) result(y)
        real, intent(in) :: x(:, :, :)
        real :: y(size(x, 1), size(x, 2), size(x, 3))

        integer, intent(in), optional :: dim
        integer :: j

        if (.not. present(dim)) error stop 1

        do j = 1, size(x, dim=1)
            y(j, :, :) = softmax_r2_sp(x(j, :, :), dim=2)
        end do

        if (size(y, 1) /= size(x, 1)) error stop 2
        if (size(y, 2) /= size(x, 2)) error stop 3
        if (size(y, 3) /= size(x, 3)) error stop 4
    end function softmax_r3_sp

    module function softmax_r2_sp(x, dim) result(y)
        real, intent(in) :: x(:, :)
        real :: y(size(x, 1), size(x, 2))

        integer, intent(in) :: dim
        y = x + 0.0 * dim
    end function softmax_r2_sp

end module arrays_op_31_mod

program arrays_op_31
    use arrays_op_31_mod, only: softmax_r3_sp
    implicit none

    real :: x(2, 3, 4), y(2, 3, 4)
    integer :: i

    x = reshape([(real(i), i = 1, size(x))], shape(x))
    y = softmax_r3_sp(x, dim=3)

    if (any(y /= x)) error stop 5
end program arrays_op_31
