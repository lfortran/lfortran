module traits_runtime_owning_04_m
    implicit none
    integer :: assignments = 0, finalizations = 0
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: Part
        integer :: n = 10
        integer, allocatable :: memo(:)
    contains
        procedure :: assign_part
        generic :: assignment(=) => assign_part
    end type
    type :: Envelope
        type(Part) :: part
    contains
        final :: finish
    end type
    implements IValue :: Envelope
        procedure, pass :: value => read_value
    end implements
contains
    subroutine assign_part(lhs, rhs)
        class(Part), intent(inout) :: lhs
        type(Part), intent(in) :: rhs
        lhs%n = lhs%n + rhs%n
        if (.not. allocated(lhs%memo)) allocate(lhs%memo(size(rhs%memo)))
        lhs%memo = rhs%memo
        assignments = assignments + 1
    end subroutine
    subroutine finish(self)
        type(Envelope), intent(inout) :: self
        finalizations = finalizations + 1
    end subroutine
    function read_value(self) result(r)
        type(Envelope), intent(in) :: self
        integer :: r
        r = self%part%n
        if (allocated(self%part%memo)) r = r + 100 * sum(self%part%memo)
    end function
end module
program traits_runtime_owning_04
    use traits_runtime_owning_04_m
    implicit none
    type(Envelope) :: source
    class(IValue), allocatable :: owner, typed, molded
    integer :: before
    source%part%n = 3
    allocate(source%part%memo(1))
    source%part%memo = [7]
    before = finalizations
    allocate(owner, source=source)
    if (owner%value() /= 703 .or. assignments /= 0) error stop 1
    if (finalizations /= before) error stop 2
    allocate(Envelope :: typed)
    allocate(molded, mold=owner)
    if (typed%value() /= 10 .or. molded%value() /= 10) error stop 3
    before = finalizations
    typed = source
    if (typed%value() /= 713 .or. assignments /= 1) error stop 4
    if (finalizations /= before + 1) error stop 5
    before = finalizations
    owner = source
    if (owner%value() /= 706 .or. assignments /= 2) error stop 6
    if (finalizations /= before + 1) error stop 7
    before = finalizations
    owner = owner
    if (owner%value() /= 712 .or. assignments /= 3) error stop 8
    if (finalizations /= before + 1) error stop 9
    source%part%memo = [8]
    if (owner%value() /= 712 .or. typed%value() /= 713) error stop 10
    before = finalizations
    deallocate(owner, typed, molded)
    if (finalizations /= before + 3) error stop 11
    deallocate(source%part%memo)
end program
