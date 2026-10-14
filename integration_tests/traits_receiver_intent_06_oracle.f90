! Standard-Fortran counterpart of traits_receiver_intent_06.f90 with single
! inheritance: an abstract Gauge with level and raise, an abstract Dial that
! extends it with turn (passed object last) and zero (intent(out)), and the
! concrete Knob and Wheel. Knob's bump stands for the message that the trait
! test's ILeft and IRight share. It checks the same values.
module traits_receiver_intent_06_oracle_m
    implicit none
    type, abstract :: Gauge
    contains
        procedure(level_iface), deferred :: level
        procedure(raise_iface), deferred :: raise
    end type
    type, abstract, extends(Gauge) :: Dial
    contains
        procedure(turn_iface), deferred, pass(dl) :: turn
        procedure(zero_iface), deferred :: zero
    end type
    abstract interface
        integer function level_iface(self)
            import :: Gauge
            class(Gauge), intent(in) :: self
        end function
        subroutine raise_iface(self)
            import :: Gauge
            class(Gauge), intent(inout) :: self
        end subroutine
        subroutine turn_iface(steps, dl)
            import :: Dial
            integer, intent(in) :: steps
            class(Dial), intent(inout) :: dl
        end subroutine
        subroutine zero_iface(self)
            import :: Dial
            class(Dial), intent(out) :: self
        end subroutine
    end interface
    type, extends(Dial) :: Knob
        integer :: n = 0
    contains
        procedure :: level => knob_level
        procedure :: raise => knob_raise
        procedure, pass(dl) :: turn => knob_turn
        procedure :: zero => knob_zero
        procedure, pass(k) :: bump => knob_bump
    end type
    type, extends(Dial) :: Wheel
        integer :: n = 0
    contains
        procedure :: level => wheel_level
        procedure :: raise => wheel_raise
        procedure, pass(dl) :: turn => wheel_turn
        procedure :: zero => wheel_zero
    end type
contains
    integer function knob_level(self)
        class(Knob), intent(in) :: self
        knob_level = self%n
    end function
    subroutine knob_raise(self)
        class(Knob), intent(inout) :: self
        self%n = self%n + 1
    end subroutine
    subroutine knob_turn(steps, dl)
        integer, intent(in) :: steps
        class(Knob), intent(inout) :: dl
        dl%n = dl%n + steps
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
    subroutine wheel_turn(steps, dl)
        integer, intent(in) :: steps
        class(Wheel), intent(inout) :: dl
        dl%n = dl%n + 10 * steps
    end subroutine
    subroutine wheel_zero(self)
        class(Wheel), intent(out) :: self
        if (self%n /= 0) error stop 91
    end subroutine
    integer function read_gauge(g)
        class(Gauge), intent(in) :: g
        read_gauge = g%level()
    end function
    subroutine raise_gauge(g)
        class(Gauge), intent(inout) :: g
        call g%raise()
    end subroutine
    subroutine use_dial(d)
        class(Dial), intent(inout) :: d
        call d%turn(2)
        call raise_gauge(d)
        if (read_gauge(d) /= d%level()) error stop 1
    end subroutine
    subroutine zero_combined(x)
        class(Dial), intent(inout) :: x
        call x%raise()
        if (read_gauge(x) == 0) error stop 2
        call x%zero()
    end subroutine
    subroutine bump_left(x)
        class(Knob), intent(inout) :: x
        call x%bump(1)
    end subroutine
    subroutine bump_right(x)
        class(Knob), intent(inout) :: x
        call x%bump(by=2)
    end subroutine
    subroutine bump_both(x)
        class(Knob), intent(inout) :: x
        call x%bump(4)
        call bump_left(x)
        call bump_right(x)
    end subroutine
    subroutine turn_generic(x)
        class(Dial), intent(inout) :: x
        call x%raise()
        call x%turn(steps=3)
    end subroutine
end module

program traits_receiver_intent_06_oracle
    use traits_receiver_intent_06_oracle_m
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
