! LFortran traits extension test; GFortran does not accept this syntax.
! Explicit receivers across trait inheritance and composition (TD-36). IGauge
! declares level with an intent(in) receiver and raise with an intent(inout)
! one; IDial extends it with turn, whose receiver dial is last. An
! intent(inout) IDial view is weakened to read-only and definable IGauge
! views, which keep the implementations selected when the view was formed.
! An anonymous combination IGauge + IReset calls raise and the intent(out)
! zero. ILeft and IRight both declare bump with an intent(inout) receiver of
! another name and position; a type that implements both binds bump once,
! and calls through either trait and through their combination reach it. A
! generic procedure constrained by IDial calls the inherited mutator through
! its type parameter. traits_receiver_intent_06_oracle.f90 is the
! standard-Fortran counterpart.
module traits_receiver_intent_06_m
    implicit none
    abstract interface :: IGauge
        integer function level(self)
            type(itself), intent(in), pass :: self
        end function
        subroutine raise(self)
            type(itself), intent(inout), pass :: self
        end subroutine
    end interface
    abstract interface, extends(IGauge) :: IDial
        subroutine turn(steps, dial)
            integer, intent(in) :: steps
            type(itself), intent(inout), pass :: dial
        end subroutine
    end interface
    abstract interface :: IReset
        subroutine zero(self)
            type(itself), intent(out), pass :: self
        end subroutine
    end interface
    abstract interface :: ILeft
        subroutine bump(self, by)
            type(itself), intent(inout), pass :: self
            integer, intent(in) :: by
        end subroutine
    end interface
    abstract interface :: IRight
        subroutine bump(by, it)
            integer, intent(in) :: by
            type(itself), intent(inout), pass :: it
        end subroutine
    end interface
    type :: Knob
        integer :: n = 0
    end type
    type :: Wheel
        integer :: n = 0
    end type
    implements (IDial + IReset) :: Knob
        procedure, pass :: level => knob_level
        procedure, pass :: raise => knob_raise
        procedure, pass :: turn => knob_turn
        procedure, pass :: zero => knob_zero
    end implements
    implements (IDial + IReset) :: Wheel
        procedure, pass :: level => wheel_level
        procedure, pass :: raise => wheel_raise
        procedure, pass(w) :: turn => wheel_turn
        procedure, pass :: zero => wheel_zero
    end implements
    implements (ILeft + IRight) :: Knob
        procedure, pass(k) :: bump => knob_bump
    end implements
contains
    integer function knob_level(self)
        class(Knob), intent(in) :: self
        knob_level = self%n
    end function
    subroutine knob_raise(self)
        class(Knob), intent(inout) :: self
        self%n = self%n + 1
    end subroutine
    subroutine knob_turn(self, steps)
        class(Knob), intent(inout) :: self
        integer, intent(in) :: steps
        self%n = self%n + steps
    end subroutine
    subroutine knob_zero(self)
        class(Knob), intent(out) :: self
        if (self%n /= 0) error stop 90
    end subroutine
    subroutine knob_bump(by, k)
        integer, intent(in) :: by
        class(Knob), intent(inout) :: k
        k%n = k%n + by
    end subroutine
    integer function wheel_level(self)
        class(Wheel), intent(in) :: self
        wheel_level = self%n
    end function
    subroutine wheel_raise(self)
        class(Wheel), intent(inout) :: self
        self%n = self%n + 10
    end subroutine
    subroutine wheel_turn(steps, w)
        integer, intent(in) :: steps
        class(Wheel), intent(inout) :: w
        w%n = w%n + 10 * steps
    end subroutine
    subroutine wheel_zero(self)
        class(Wheel), intent(out) :: self
        if (self%n /= 0) error stop 91
    end subroutine
    integer function read_gauge(g)
        class(IGauge), intent(in) :: g
        read_gauge = g%level()
    end function
    subroutine raise_gauge(g)
        class(IGauge), intent(inout) :: g
        call g%raise()
    end subroutine
    subroutine use_dial(d)
        class(IDial), intent(inout) :: d
        call d%turn(2)
        call raise_gauge(d)
        if (read_gauge(d) /= d%level()) error stop 1
    end subroutine
    subroutine zero_combined(x)
        class(IGauge + IReset), intent(inout) :: x
        call x%raise()
        if (read_gauge(x) == 0) error stop 2
        call x%zero()
    end subroutine
    subroutine bump_left(x)
        class(ILeft), intent(inout) :: x
        call x%bump(1)
    end subroutine
    subroutine bump_right(x)
        class(IRight), intent(inout) :: x
        call x%bump(by=2)
    end subroutine
    subroutine bump_both(x)
        class(ILeft + IRight), intent(inout) :: x
        call x%bump(4)
        call bump_left(x)
        call bump_right(x)
    end subroutine
    subroutine turn_generic{IDial :: D}(x)
        type(D), intent(inout) :: x
        call x%raise()
        call x%turn(steps=3)
    end subroutine
end module

program traits_receiver_intent_06
    use traits_receiver_intent_06_m
    implicit none
    type(Knob) :: k
    type(Wheel) :: w
    call use_dial(k)
    call use_dial(w)
    if (k%n /= 3 .or. w%n /= 30) error stop 11
    call zero_combined(k)
    call zero_combined(w)
    if (k%n /= 0 .or. w%n /= 0) error stop 12
    call bump_both(k)
    if (k%n /= 7) error stop 13
    call turn_generic(w)
    call turn_generic(k)
    if (w%n /= 40 .or. k%n /= 11) error stop 14
    print '(a)', 'traits_receiver_intent_06 ok'
end program
