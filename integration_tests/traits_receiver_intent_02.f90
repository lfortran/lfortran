! LFortran traits extension test; GFortran does not accept this syntax.
! Explicit receivers (TD-36) have any name and position: IScale's scale takes
! its receiver this second, weighted its receiver obj last, and shift its
! receiver self first. Each implementation chooses its own passed object,
! first, in the middle, last or named, independently of the message, and the
! other dummies correspond in order. Calls through a trait object or a
! constrained type parameter use the message's names for keyword arguments,
! never the receiver's; current's receiver is named at. kind_code has no
! receiver: NOPASS implementations, selected through the object, implement
! it. Direct calls on concrete objects pass the object to each
! implementation's own passed object.
! traits_receiver_intent_02_oracle.f90 is the standard-Fortran counterpart.
module traits_receiver_intent_02_m
    implicit none
    abstract interface :: IScale
        subroutine scale(factor, this)
            integer, intent(in) :: factor
            type(itself), intent(inout), pass :: this
        end subroutine
        integer function weighted(weight, offset, obj)
            integer, intent(in) :: weight, offset
            type(itself), intent(in), pass :: obj
        end function
        subroutine shift(self, by, total)
            type(itself), intent(inout), pass :: self
            integer, intent(in) :: by
            integer, intent(out) :: total
        end subroutine
        integer function kind_code(base)
            integer, intent(in) :: base
        end function
        integer function current(at)
            type(itself), intent(in), pass :: at
        end function
    end interface
    type :: Linear
        integer :: v = 1
    end type
    type :: Doubling
        integer :: v = 1
    end type
    implements IScale :: Linear
        procedure, pass :: scale => linear_scale
        procedure, pass(me) :: weighted => linear_weighted
        procedure, pass(it) :: shift => linear_shift
        procedure, nopass :: kind_code => linear_code
        procedure, pass :: current => linear_current
    end implements
    implements IScale :: Doubling
        procedure, pass(this) :: scale => doubling_scale
        procedure, pass :: weighted => doubling_weighted
        procedure, pass(self) :: shift => doubling_shift
        procedure, nopass :: kind_code => doubling_code
        procedure, pass(x) :: current => doubling_current
    end implements
contains
    subroutine linear_scale(self, f)
        class(Linear), intent(inout) :: self
        integer, intent(in) :: f
        self%v = self%v * f
    end subroutine
    integer function linear_weighted(w, me, o)
        integer, intent(in) :: w
        class(Linear), intent(in) :: me
        integer, intent(in) :: o
        linear_weighted = w * me%v + o
    end function
    subroutine linear_shift(b, t, it)
        integer, intent(in) :: b
        integer, intent(out) :: t
        class(Linear), intent(inout) :: it
        it%v = it%v + b
        t = it%v
    end subroutine
    integer function linear_code(b)
        integer, intent(in) :: b
        linear_code = b + 1
    end function
    integer function linear_current(self)
        class(Linear), intent(in) :: self
        linear_current = self%v
    end function
    subroutine doubling_scale(factor, this)
        integer, intent(in) :: factor
        class(Doubling), intent(inout) :: this
        this%v = this%v * factor * 2
    end subroutine
    integer function doubling_weighted(obj, weight, offset)
        class(Doubling), intent(in) :: obj
        integer, intent(in) :: weight, offset
        doubling_weighted = weight * obj%v * 2 + offset
    end function
    subroutine doubling_shift(by, self, total)
        integer, intent(in) :: by
        class(Doubling), intent(inout) :: self
        integer, intent(out) :: total
        self%v = self%v + 2 * by
        total = self%v
    end subroutine
    integer function doubling_code(base)
        integer, intent(in) :: base
        doubling_code = base + 2
    end function
    integer function doubling_current(x)
        class(Doubling), intent(in) :: x
        doubling_current = x%v
    end function
    ! expected: v after scale, weighted, v and total after shift, kind_code.
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
    ! A read-only view calls the intent(in) and receiverless messages.
    integer function observe(x, base)
        class(IScale), intent(in) :: x
        integer, intent(in) :: base
        observe = x%current() + 0 * x%kind_code(base)
    end function
    subroutine exercise_generic{IScale :: T}(x, expected)
        type(T), intent(inout) :: x
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

program traits_receiver_intent_02
    use traits_receiver_intent_02_m
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
    ! Direct calls: each implementation's passed object receives c.
    call c%scale(5)
    if (c%weighted(2, 1) /= 11) error stop 24
    call c%shift(2, total)
    if (c%v /= 7 .or. total /= 7 .or. c%kind_code(1) /= 2) error stop 25
    if (c%current() /= 7 .or. observe(c, 0) /= 7) error stop 26
    print '(a)', 'traits_receiver_intent_02 ok'
end program
