module gpu_optional_launch_02_mod
    implicit none
contains
    subroutine apply(n, required, bias)
        integer, intent(in) :: n
        integer, intent(out) :: required(n)
        integer, optional, intent(inout) :: bias(:)
        integer :: i
        do concurrent (i = 1:n)
            required(i) = i
            if (present(bias)) bias(i) = bias(i) + 1
        end do
        if (present(bias)) then
            if (bias(1) /= 11) error stop 1
            bias(1) = bias(1) + 20
        end if
    end subroutine

    subroutine apply_target(n, bias)
        integer, intent(in) :: n
        integer, optional, target, intent(inout) :: bias(:)
        integer :: i
        do concurrent (i = 1:n)
            if (present(bias)) bias(i) = bias(i) + 1
        end do
        if (present(bias)) then
            if (bias(1) /= 32) error stop 2
            bias(1) = bias(1) + 20
        end if
    end subroutine
end module
