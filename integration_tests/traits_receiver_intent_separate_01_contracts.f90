! Explicit receivers across separately compiled units (TD-36): the trait of
! traits_receiver_intent_separate_01. value has an intent(in) receiver, bump
! and take intent(inout) ones and reset an intent(out) receiver after its
! other argument.
module traits_receiver_intent_separate_01_contracts_m
    implicit none
    abstract interface :: ICounter
        integer function value(self)
            type(itself), intent(in), pass :: self
        end function value
        subroutine bump(self, by)
            type(itself), intent(inout), pass :: self
            integer, intent(in) :: by
        end subroutine bump
        integer function take(self)
            type(itself), intent(inout), pass :: self
        end function take
        subroutine reset(seed, self)
            integer, intent(in) :: seed
            type(itself), intent(out), pass :: self
        end subroutine reset
    end interface ICounter
end module traits_receiver_intent_separate_01_contracts_m
