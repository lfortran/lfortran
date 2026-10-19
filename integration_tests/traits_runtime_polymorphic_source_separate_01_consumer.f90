! Compiled against the contract only, before any implementation exists.
module traits_runtime_polymorphic_source_separate_01_consumer_m
    use traits_runtime_polymorphic_source_separate_01_contracts_m, only: IStep
    implicit none
    private
    public :: advance, peek, advance_pointer
contains
    subroutine advance(s, y)
        class(IStep), intent(inout) :: s
        real(8), intent(inout) :: y
        call s%step(y)
    end subroutine

    integer function peek(s)
        class(IStep), intent(in) :: s
        peek = s%count()
    end function

    subroutine advance_pointer(p, y)
        class(IStep), pointer, intent(inout) :: p
        real(8), intent(inout) :: y
        call advance(p, y)
    end subroutine
end module
