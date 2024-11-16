! test to ensure that class procedure without "deferred"
! and without implementation doesn't raise semantic error
! this is an MRE extracted from 'neural-fortran' repository
module procedure_07_module
    implicit none

    private

    public :: layer

    type :: layer
        real, allocatable :: weights(:)
        real :: learning_rate = 0.01

        contains

        ! class procedure without "DEFERRED" and without
        ! an implementation
        procedure, private :: backward_1d
        generic :: backward => backward_1d
    end type

    interface backward
        module subroutine backward_1d(self, previous, gradient)
            class(layer), intent(inout) :: self
            class(layer), intent(in) :: previous
            real, intent(in) :: gradient(:)
        end subroutine
    end interface

    contains
end module
