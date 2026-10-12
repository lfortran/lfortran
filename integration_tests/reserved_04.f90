! static, initial, endinitial and itself remain ordinary identifiers, also
! as type-bound procedure names, next to an ordinary type-bound operator.
module static
    implicit none
    type :: holder
        integer :: static = 1
        integer :: initial = 2
        integer :: n = 3
    contains
        procedure :: itself
        procedure, nopass :: initial_np => static_np
        generic :: g => itself
        generic :: operator(*) => times
        procedure, pass :: times
    end type holder
contains
    integer function static_fn(static) result(r)
        integer, intent(in) :: static
        r = static + 1
    end function static_fn
    subroutine initial(endinitial)
        integer, intent(inout) :: endinitial
        endinitial = endinitial + 10
    end subroutine initial
    integer function itself(self)
        class(holder), intent(in) :: self
        itself = self%n
    end function itself
    integer function static_np()
        static_np = 9
    end function static_np
    function times(lhs, rhs) result(res)
        class(holder), intent(in) :: lhs, rhs
        type(holder) :: res
        res%n = lhs%n * rhs%n
    end function times
end module static

program reserved_04
    use static
    implicit none
    integer :: static_v, endinitial, initial_v, itself_v
    integer :: static2(3)
    type(holder) :: h, k
    static_v = static_fn(static=4)
    endinitial = 1
    call initial(endinitial)
    initial_v = h%static + h%initial
    static2 = [1, 2, 3]
    do itself_v = 1, 2
        endinitial = endinitial + itself_v
    end do
    k = h * h
    if (static_v /= 5) error stop 1
    if (endinitial /= 14) error stop 2
    if (initial_v /= 3) error stop 3
    if (sum(static2) /= 6) error stop 4
    if (h%itself() /= 3 .or. h%g() /= 3) error stop 5
    if (h%initial_np() /= 9) error stop 6
    if (k%n /= 9) error stop 7
    print '(a)', 'reserved_04 ok'
end program reserved_04
