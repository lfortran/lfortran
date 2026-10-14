! LFortran traits extension test; GFortran does not accept this syntax.
! Explicit receivers (TD-36) whose INTENT is given by a separate INTENT
! statement, before or after the declaration with the PASS attribute. Each
! receiver has exactly the stated intent: the implementations bind passed
! objects with the same INTENT, a read-only view calls the intent(in)
! message, an intent(inout) view the others, and the intent(out) receiver is
! default-initialized on entry. Direct, static generic and view calls agree.
module traits_receiver_intent_10_m
    implicit none
    abstract interface :: ITally
        integer function total(self)
            intent(in) :: self
            type(itself), pass :: self
        end function
        subroutine add(by, self)
            integer, intent(in) :: by
            type(itself), pass :: self
            intent(inout) :: self
        end subroutine
        subroutine clear(self)
            intent(out) :: self
            type(itself), pass :: self
        end subroutine
    end interface
    type :: Tally
        integer :: n = 0
        integer :: base = 7
    end type
    implements ITally :: Tally
        procedure, pass :: total => tally_total
        procedure, pass(t) :: add => tally_add
        procedure, pass :: clear => tally_clear
    end implements
contains
    integer function tally_total(self)
        class(Tally), intent(in) :: self
        tally_total = self%n
    end function
    subroutine tally_add(by, t)
        integer, intent(in) :: by
        class(Tally), intent(inout) :: t
        t%n = t%n + by
    end subroutine
    subroutine tally_clear(self)
        class(Tally), intent(out) :: self
        if (self%n /= 0 .or. self%base /= 7) error stop 90
        self%n = self%base
    end subroutine
    integer function observe(v)
        class(ITally), intent(in) :: v
        observe = v%total()
    end function
    subroutine bump(v, by)
        class(ITally), intent(inout) :: v
        integer, intent(in) :: by
        call v%add(by)
    end subroutine
    subroutine reset(v)
        class(ITally), intent(inout) :: v
        call v%clear()
    end subroutine
    integer function cycle_generic{ITally :: T}(x, by)
        type(T), intent(inout) :: x
        integer, intent(in) :: by
        call x%clear()
        call x%add(by)
        cycle_generic = x%total()
    end function
end module

program traits_receiver_intent_10
    use traits_receiver_intent_10_m
    implicit none
    type(Tally) :: t

    call t%add(2)
    if (t%total() /= 2) error stop 1
    call bump(t, 3)
    if (observe(t) /= 5 .or. t%n /= 5) error stop 2
    t%base = 99
    call reset(t)
    if (t%n /= 7 .or. t%base /= 7) error stop 3
    t%n = 40
    call t%clear()
    if (t%n /= 7) error stop 4
    if (cycle_generic(t, 4) /= 11 .or. t%n /= 11) error stop 5
    print '(a)', 'traits_receiver_intent_10 ok'
end program
