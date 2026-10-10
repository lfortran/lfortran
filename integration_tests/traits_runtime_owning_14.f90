! Image termination releases the live direct trait owners of the main program
! and of a module, with their nested owned payloads, without running FINAL.
! Every lane checks that no FINAL runs at image exit; the leak lane checks the
! release.
module traits_runtime_owning_14_m
    implicit none
    logical :: body_finished = .false.
    integer :: finals = 0

    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function value
    end interface IValue

    type, sealed, implements(IValue) :: Leaf
        integer :: n = 2
    contains
        procedure, pass :: value => leaf_value
        final :: leaf_final
    end type Leaf

    type, sealed, implements(IValue) :: Outer
        integer :: n = 10
        class(IValue), allocatable :: inner
    contains
        procedure, pass :: value => outer_value
        final :: outer_final
    end type Outer

    class(IValue), allocatable :: module_owner

contains

    integer function leaf_value(self) result(r)
        type(Leaf), intent(in) :: self
        r = self%n
    end function leaf_value

    integer function outer_value(self) result(r)
        type(Outer), intent(in) :: self
        r = self%n
        if (allocated(self%inner)) r = r + self%inner%value()
    end function outer_value

    subroutine leaf_final(self)
        type(Leaf), intent(inout) :: self
        if (body_finished) error stop 91
        finals = finals + 1
    end subroutine leaf_final

    subroutine outer_final(self)
        type(Outer), intent(inout) :: self
        if (body_finished) error stop 92
        finals = finals + 1
    end subroutine outer_final

    ! Gives the caller's owner an Outer whose component owns a Leaf.
    subroutine fill(slot, n, m)
        class(IValue), allocatable, intent(inout) :: slot
        integer, intent(in) :: n, m
        type(Outer) :: part
        type(Leaf) :: piece
        piece%n = m
        part%n = n
        allocate(part%inner, source=piece)
        allocate(slot, source=part)
    end subroutine fill

    ! A live local owner is finalized when its procedure returns.
    subroutine local_scope()
        class(IValue), allocatable :: local
        allocate(Leaf :: local)
    end subroutine local_scope

end module traits_runtime_owning_14_m

program traits_runtime_owning_14
    use traits_runtime_owning_14_m
    implicit none
    class(IValue), allocatable :: direct, empty
    integer :: before

    call fill(direct, 10, 5)
    call fill(module_owner, 20, 7)
    if (direct%value() /= 15) error stop 1
    if (module_owner%value() /= 27) error stop 2
    if (allocated(empty)) error stop 3
    before = finals
    call local_scope()
    if (finals /= before + 1) error stop 4
    print '(a)', 'traits_runtime_owning_14: live owners left to image termination'
    body_finished = .true.
end program traits_runtime_owning_14
