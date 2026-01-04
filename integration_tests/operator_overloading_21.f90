module operator_overloading_21_mod
    implicit none

    type :: temp
        integer :: v = 0
    contains
        procedure, pass :: not_equal
        generic :: operator(/=) => not_equal
    end type temp

contains

    elemental logical function not_equal(lhs, rhs)
        class(temp), intent(in) :: lhs, rhs
        not_equal = lhs%v /= rhs%v
    end function not_equal

end module operator_overloading_21_mod


program operator_overloading_21
    use operator_overloading_21_mod
    implicit none

    type(temp), allocatable :: a(:), b(:)

    allocate(a(3))
    allocate(b(3))
    a%v = [1, 2, 3]
    b%v = [1, 0, 3]

    print *, "any(a /= b) =", any(a /= b)
    if (.not. any(a /= b)) then
        error stop
    end if

    b%v = [1, 2, 3]
    print *, "any(a /= b) =", any(a /= b)
    if (any(a /= b)) then
        error stop
    end if
end program operator_overloading_21
