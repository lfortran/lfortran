! The first provider: a finalizable Counter with an allocatable component,
! implemented with passed objects in the first position.
module traits_receiver_intent_separate_01_a_m
    use iso_c_binding, only: c_ptr, c_loc, c_null_ptr
    use traits_receiver_intent_separate_01_contracts_m, only: ICounter
    implicit none
    integer :: a_finals = 0, a_finals_with_log = 0
    type(c_ptr) :: a_bump_seen = c_null_ptr, a_reset_seen = c_null_ptr
    type :: Counter
        integer :: n = 0
        integer, allocatable :: log(:)
    contains
        final :: a_final
    end type Counter
    implements ICounter :: Counter
        procedure, pass :: value => a_value
        procedure, pass :: bump => a_bump
        procedure, pass :: take => a_take
        procedure, pass(self) :: reset => a_reset
    end implements Counter
contains
    integer function a_value(self)
        class(Counter), intent(in) :: self
        a_value = self%n
    end function a_value
    subroutine a_bump(self, by)
        class(Counter), intent(inout), target :: self
        integer, intent(in) :: by
        integer, allocatable :: grown(:)
        a_bump_seen = c_loc(self%n)
        self%n = self%n + by
        if (.not. allocated(self%log)) allocate(self%log(0))
        allocate(grown(size(self%log) + 1))
        grown(:size(self%log)) = self%log
        grown(size(grown)) = by
        call move_alloc(grown, self%log)
    end subroutine a_bump
    integer function a_take(self)
        class(Counter), intent(inout) :: self
        a_take = self%n
        self%n = 0
    end function a_take
    subroutine a_reset(self, seed)
        class(Counter), intent(out), target :: self
        integer, intent(in) :: seed
        if (self%n /= 0 .or. allocated(self%log)) error stop 711
        a_reset_seen = c_loc(self%n)
        self%n = seed
        allocate(self%log(0))
    end subroutine a_reset
    subroutine a_final(self)
        type(Counter), intent(inout) :: self
        a_finals = a_finals + 1
        if (allocated(self%log)) a_finals_with_log = a_finals_with_log + 1
    end subroutine a_final
end module traits_receiver_intent_separate_01_a_m
