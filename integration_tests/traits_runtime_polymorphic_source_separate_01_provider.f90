! Euler adopts IStep. Its run binding and drive form views of class(Euler)
! objects here, before the extension that overrides step is compiled.
module traits_runtime_polymorphic_source_separate_01_provider_m
    use traits_runtime_polymorphic_source_separate_01_contracts_m, only: IStep
    use traits_runtime_polymorphic_source_separate_01_consumer_m, only: advance, peek
    implicit none
    private
    public :: Euler, drive, count_of

    type, implements(IStep) :: Euler
        real(8) :: h = 0.5d0
        integer :: nsteps = 0
    contains
        procedure :: step => euler_step
        procedure :: count => euler_count
        procedure :: run => euler_run
    end type
contains
    subroutine euler_step(self, y)
        class(Euler), intent(inout) :: self
        real(8), intent(inout) :: y
        y = y + self%h
        self%nsteps = self%nsteps + 1
    end subroutine

    integer function euler_count(self)
        class(Euler), intent(in) :: self
        euler_count = self%nsteps
    end function

    subroutine euler_run(self, y)
        class(Euler), intent(inout) :: self
        real(8), intent(inout) :: y
        call advance(self, y)
    end subroutine

    subroutine drive(e, y)
        class(Euler), intent(inout) :: e
        real(8), intent(inout) :: y
        call advance(e, y)
    end subroutine

    integer function count_of(e)
        class(Euler), intent(in) :: e
        count_of = peek(e)
    end function
end module
