! Standard-Fortran counterpart of traits_receiver_intent_02.f90: deferred
! bindings whose passed objects are a named second, last or first dummy, and
! a NOPASS binding, called through class(IScale) dummies with keyword
! arguments and on concrete objects. Standard overriding keeps each passed
! object's position, so only the trait test varies the implementations'
! positions; the values checked are the same.
module traits_receiver_intent_02_oracle_m
    implicit none
    type, abstract :: IScale
    contains
        procedure(scale_iface), deferred, pass(this) :: scale
        procedure(weighted_iface), deferred, pass(obj) :: weighted
        procedure(shift_iface), deferred, pass(self) :: shift
        procedure(kind_code_iface), deferred, nopass :: kind_code
        procedure(current_iface), deferred, pass(at) :: current
    end type
    abstract interface
        subroutine scale_iface(factor, this)
            import :: IScale
            integer, intent(in) :: factor
            class(IScale), intent(inout) :: this
        end subroutine
        integer function weighted_iface(weight, offset, obj)
            import :: IScale
            integer, intent(in) :: weight, offset
            class(IScale), intent(in) :: obj
        end function
        subroutine shift_iface(self, by, total)
            import :: IScale
            class(IScale), intent(inout) :: self
            integer, intent(in) :: by
            integer, intent(out) :: total
        end subroutine
        integer function kind_code_iface(base)
            integer, intent(in) :: base
        end function
        integer function current_iface(at)
            import :: IScale
            class(IScale), intent(in) :: at
        end function
    end interface
    type, extends(IScale) :: Linear
        integer :: v = 1
    contains
        procedure, pass(this) :: scale => linear_scale
        procedure, pass(obj) :: weighted => linear_weighted
        procedure, pass(self) :: shift => linear_shift
        procedure, nopass :: kind_code => linear_code
        procedure, pass(at) :: current => linear_current
    end type
    type, extends(IScale) :: Doubling
        integer :: v = 1
    contains
        procedure, pass(this) :: scale => doubling_scale
        procedure, pass(obj) :: weighted => doubling_weighted
        procedure, pass(self) :: shift => doubling_shift
        procedure, nopass :: kind_code => doubling_code
        procedure, pass(at) :: current => doubling_current
    end type
contains
    subroutine linear_scale(factor, this)
        integer, intent(in) :: factor
        class(Linear), intent(inout) :: this
        this%v = this%v * factor
    end subroutine
    integer function linear_weighted(weight, offset, obj)
        integer, intent(in) :: weight, offset
        class(Linear), intent(in) :: obj
        linear_weighted = weight * obj%v + offset
    end function
    subroutine linear_shift(self, by, total)
        class(Linear), intent(inout) :: self
        integer, intent(in) :: by
        integer, intent(out) :: total
        self%v = self%v + by
        total = self%v
    end subroutine
    integer function linear_code(base)
        integer, intent(in) :: base
        linear_code = base + 1
    end function
    integer function linear_current(at)
        class(Linear), intent(in) :: at
        linear_current = at%v
    end function
    subroutine doubling_scale(factor, this)
        integer, intent(in) :: factor
        class(Doubling), intent(inout) :: this
        this%v = this%v * factor * 2
    end subroutine
    integer function doubling_weighted(weight, offset, obj)
        integer, intent(in) :: weight, offset
        class(Doubling), intent(in) :: obj
        doubling_weighted = weight * obj%v * 2 + offset
    end function
    subroutine doubling_shift(self, by, total)
        class(Doubling), intent(inout) :: self
        integer, intent(in) :: by
        integer, intent(out) :: total
        self%v = self%v + 2 * by
        total = self%v
    end subroutine
    integer function doubling_code(base)
        integer, intent(in) :: base
        doubling_code = base + 2
    end function
    integer function doubling_current(at)
        class(Doubling), intent(in) :: at
        doubling_current = at%v
    end function
    subroutine exercise(x, expected)
        class(IScale), intent(inout) :: x
        integer, intent(in) :: expected(5)
        integer :: total
        call x%scale(3)
        if (observe(x, 0) /= expected(1)) error stop 1
        if (x%weighted(offset=5, weight=2) /= expected(2)) error stop 2
        call x%shift(total=total, by=4)
        if (observe(x, 0) /= expected(3) .or. total /= expected(4)) error stop 3
        if (x%kind_code(base=10) /= expected(5)) error stop 4
    end subroutine
    integer function observe(x, base)
        class(IScale), intent(in) :: x
        integer, intent(in) :: base
        observe = x%current() + 0 * x%kind_code(base)
    end function
    subroutine exercise_generic(x, expected)
        class(IScale), intent(inout) :: x
        integer, intent(in) :: expected(5)
        integer :: total
        call x%scale(factor=3)
        if (x%current() /= expected(1)) error stop 11
        if (x%weighted(weight=2, offset=5) /= expected(2)) error stop 12
        call x%shift(4, total)
        if (x%current() /= expected(3) .or. total /= expected(4)) error stop 13
        if (x%kind_code(10) /= expected(5)) error stop 14
    end subroutine
end module

program traits_receiver_intent_02_oracle
    use traits_receiver_intent_02_oracle_m
    implicit none
    type(Linear) :: a, b, c
    type(Doubling) :: d, e
    integer :: total
    call exercise(a, [3, 11, 7, 7, 11])
    if (a%v /= 7) error stop 21
    call exercise(d, [6, 29, 14, 14, 12])
    if (d%v /= 14) error stop 22
    call exercise_generic(b, [3, 11, 7, 7, 11])
    call exercise_generic(e, [6, 29, 14, 14, 12])
    if (b%v /= 7 .or. e%v /= 14) error stop 23
    call c%scale(5)
    if (c%weighted(2, 1) /= 11) error stop 24
    call c%shift(2, total)
    if (c%v /= 7 .or. total /= 7 .or. c%kind_code(1) /= 2) error stop 25
    if (c%current() /= 7 .or. observe(c, 0) /= 7) error stop 26
    print '(a)', 'traits_receiver_intent_02 ok'
end program
