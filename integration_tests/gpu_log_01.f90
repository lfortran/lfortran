module gpu_log_helpers
    use iso_fortran_env, only: real32
    implicit none
contains
    pure real(real32) function scalar_log(x) result(y)
        real(real32), intent(in) :: x
        block
            real(real32) :: value
            value = x
            y = log(value)
        end block
    end function

    pure real(real32) function evaluate(x) result(y)
        real(real32), intent(in) :: x
        y = scalar_log(x)
    end function
end module

program gpu_log_01
    use gpu_log_helpers, only: evaluate, real32
    implicit none
    real(real32), parameter :: expected(2) = &
        [0.69314718056_real32, 1.38629436112_real32]
    real(real32) :: x(2), direct(2), helper(2), host(2)
    integer :: i
    do i = 1, 2
        x(i) = 2.0_real32 * real(i, real32)
        host(i) = evaluate(x(i))
    end do
    do concurrent (i = 1:2)
        direct(i) = log(x(i))
        helper(i) = evaluate(x(i))
    end do
    do i = 1, 2
        if (.not. (abs(direct(i) - expected(i)) <= 1.e-6_real32)) error stop 1
        if (.not. (abs(helper(i) - expected(i)) <= 1.e-6_real32)) error stop 2
        if (.not. (abs(host(i) - expected(i)) <= 1.e-6_real32)) error stop 3
    end do
    print *, "scalar log passed"
end program
