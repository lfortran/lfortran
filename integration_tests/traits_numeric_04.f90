! Traits are an LFortran extension; ordering must hold for every member.
module traits_numeric_04_m
    use iso_fortran_env, only: real64
    implicit none

    abstract interface :: INumeric
        integer | real(real64)
    end interface INumeric

contains

    function strictly_between{INumeric :: T}(x, lower, upper) result(answer)
        type(T), intent(in) :: x, lower, upper
        logical :: answer
        answer = x > lower .and. x < upper
    end function strictly_between
end module traits_numeric_04_m

program traits_numeric_04
    use traits_numeric_04_m, only: strictly_between
    use iso_fortran_env, only: real64
    implicit none

    if (.not. strictly_between(2, 1, 3)) error stop
    if (.not. strictly_between{integer}(2, 1, 3)) error stop
    if (strictly_between(1, 1, 3)) error stop
    if (strictly_between(3, 1, 3)) error stop
    if (.not. strictly_between(2.5_real64, 2.0_real64, 3.0_real64)) error stop
    if (.not. strictly_between{real(real64)}(2.5_real64, 2.0_real64, 3.0_real64)) error stop
    if (strictly_between(3.0_real64, 2.0_real64, 3.0_real64)) error stop
end program traits_numeric_04
