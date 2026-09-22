! An array constructor assigned to an array is a value the compiler measures
! the shape of. Measuring it must not evaluate the elements a second time: an
! element whose argument calls a function needs a statement of its own to
! evaluate that call into, and running that statement twice would call the
! function twice.
module array_constructor_08_m
    implicit none
    ! counts the calls to `bump()`
    integer :: n_calls = 0
contains
    function bump() result(res)
        integer :: res
        n_calls = n_calls + 1
        res = 5
    end function
    function twice(k) result(res)
        integer, intent(in) :: k
        integer :: res
        res = 2 * k
    end function
end module

program array_constructor_08
    use array_constructor_08_m
    implicit none
    integer :: a(3)

    a = [ twice(bump()), 1, 2 ]
    if (n_calls /= 1) error stop
    if (a(1) /= 10 .or. a(2) /= 1 .or. a(3) /= 2) error stop

    n_calls = 0
    a = [ 1, twice(bump()), 2 ]
    if (n_calls /= 1) error stop
    if (a(1) /= 1 .or. a(2) /= 10 .or. a(3) /= 2) error stop

    print *, "ok"
end program
