! Basic namespace import: variables, subroutines and functions are accessed
! only through the module name, and do not clash with local names.
module namespace_modules_01_utils
    implicit none
    integer :: counter = 0
    real :: x = 1.5
contains
    subroutine increment(n)
        integer, intent(in) :: n
        counter = counter + n
    end subroutine

    integer function twice(i)
        integer, intent(in) :: i
        twice = 2*i
    end function

    subroutine scale(a, factor)
        real, intent(inout) :: a
        real, intent(in), optional :: factor
        if (present(factor)) then
            a = a*factor
        else
            a = a*10
        end if
    end subroutine
end module

program namespace_modules_01
    use, namespace :: namespace_modules_01_utils
    implicit none
    ! The module entities are not accessible without qualification, so these
    ! local names do not clash with them.
    integer :: counter
    real :: x
    integer :: twice

    counter = 100
    x = -3.0
    twice = 7

    ! Read module variables
    if (namespace_modules_01_utils%counter /= 0) error stop
    if (abs(namespace_modules_01_utils%x - 1.5) > 1e-6) error stop

    ! Call module subroutines, write module variables
    call namespace_modules_01_utils%increment(5)
    call namespace_modules_01_utils%increment(n=2)
    if (namespace_modules_01_utils%counter /= 7) error stop
    namespace_modules_01_utils%counter = namespace_modules_01_utils%counter + 1
    if (namespace_modules_01_utils%counter /= 8) error stop

    ! Call module functions, including in nested expressions
    if (namespace_modules_01_utils%twice(21) /= 42) error stop
    if (namespace_modules_01_utils%twice(namespace_modules_01_utils%twice(3)) &
            /= 12) error stop

    ! Optional and keyword arguments work as usual
    call namespace_modules_01_utils%scale(namespace_modules_01_utils%x)
    if (abs(namespace_modules_01_utils%x - 15.0) > 1e-5) error stop
    call namespace_modules_01_utils%scale(factor=2.0, &
        a=namespace_modules_01_utils%x)
    if (abs(namespace_modules_01_utils%x - 30.0) > 1e-5) error stop

    ! The local entities are untouched
    if (counter /= 100) error stop
    if (abs(x + 3.0) > 1e-6) error stop
    if (twice /= 7) error stop

    print *, namespace_modules_01_utils%counter, namespace_modules_01_utils%x
end program
