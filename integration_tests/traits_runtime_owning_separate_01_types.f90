module traits_runtime_owning_separate_01_types_m
    implicit none
    private
    public :: Payload, shared_finalizations
    integer :: shared_finalizations = 0
    type :: Payload
        integer :: n
    contains
        final :: finish
    end type
contains
    subroutine finish(self)
        type(Payload), intent(inout) :: self
        shared_finalizations = shared_finalizations + 1
    end subroutine
end module
