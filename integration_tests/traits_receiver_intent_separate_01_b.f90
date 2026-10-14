! The second provider: a same-named Counter with a default-initialized scale,
! implemented with passed objects named me and it in the last position.
module traits_receiver_intent_separate_01_b_m
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr
    use traits_receiver_intent_separate_01_contracts_m, only: ICounter
    implicit none
    type(c_ptr) :: b_bump_seen = c_null_ptr, b_reset_seen = c_null_ptr
    type :: Counter
        integer :: n = 0
        integer :: scale = 2
    end type Counter
    implements ICounter :: Counter
        procedure, pass(me) :: reset => b_reset
        procedure, pass(it) :: bump => b_bump
        procedure, pass(me) :: take => b_take
        procedure, pass(me) :: value => b_value
    end implements Counter
contains
    integer function b_value(me)
        class(Counter), intent(in) :: me
        b_value = me%scale * me%n
    end function b_value
    subroutine b_bump(by, it)
        integer, intent(in) :: by
        class(Counter), intent(inout), target :: it
        b_bump_seen = c_loc(it%n)
        it%n = it%n + by
    end subroutine b_bump
    integer function b_take(me)
        class(Counter), intent(inout) :: me
        b_take = me%scale * me%n
        me%n = 0
    end function b_take
    subroutine b_reset(seed, me)
        integer, intent(in) :: seed
        class(Counter), intent(out), target :: me
        if (me%n /= 0 .or. me%scale /= 2) error stop 721
        b_reset_seen = c_loc(me%n)
        me%n = seed
    end subroutine b_reset
end module traits_receiver_intent_separate_01_b_m
