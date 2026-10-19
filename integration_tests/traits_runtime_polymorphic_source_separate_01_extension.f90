! An extension of Euler in another module that overrides step and count.
module traits_runtime_polymorphic_source_separate_01_extension_m
    use traits_runtime_polymorphic_source_separate_01_provider_m, only: Euler
    implicit none
    private
    public :: TracedEuler

    type, extends(Euler) :: TracedEuler
        integer, allocatable :: log(:)
    contains
        procedure :: step => traced_step
        procedure :: count => traced_count
    end type
contains
    subroutine traced_step(self, y)
        class(TracedEuler), intent(inout) :: self
        real(8), intent(inout) :: y
        integer, allocatable :: log(:)
        y = y + 10*self%h
        self%nsteps = self%nsteps + 1
        allocate(log(size(self%log) + 1))
        log(:size(self%log)) = self%log
        log(size(log)) = self%nsteps
        call move_alloc(log, self%log)
    end subroutine

    integer function traced_count(self)
        class(TracedEuler), intent(in) :: self
        traced_count = 1000*size(self%log) + self%nsteps
    end function
end module
