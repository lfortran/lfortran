module traits_runtime_owning_03_m
    implicit none
    integer :: finalized = 0
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type, abstract :: Base
    contains
        procedure(measure_signature), deferred :: measure
    end type
    abstract interface
        function measure_signature(self) result(r)
            import :: Base
            class(Base), intent(in) :: self
            integer :: r
        end function
    end interface
    type, extends(Base) :: Child
        integer, allocatable :: numbers(:)
    contains
        procedure :: measure => measure_child
        final :: finish_child
    end type
    type :: Envelope
        class(Base), allocatable :: part
    end type
    implements IValue :: Envelope
        procedure, pass :: value => measure_envelope
    end implements
contains
    function measure_child(self) result(r)
        class(Child), intent(in) :: self
        integer :: r
        r = self%numbers(1)
    end function
    subroutine finish_child(self)
        type(Child), intent(inout) :: self
        finalized = finalized + 1
    end subroutine
    function measure_envelope(self) result(r)
        class(Envelope), intent(in) :: self
        integer :: r
        r = -1
        if (allocated(self%part)) r = self%part%measure()
    end function
end module

program traits_runtime_owning_03
    use traits_runtime_owning_03_m
    implicit none
    type(Envelope) :: source
    class(IValue), allocatable :: owner, copy
    integer :: before
    allocate(Child :: source%part)
    select type (part => source%part)
    type is (Child)
        allocate(part%numbers(1))
        part%numbers = 17
    end select
    before = finalized
    allocate(owner, source=source)
    if (finalized /= before) error stop 1
    if (owner%value() /= 17) error stop 2
    allocate(copy, mold=owner)
    if (copy%value() /= -1) error stop 3
    deallocate(copy)
    before = finalized
    owner = owner
    if (finalized /= before + 1) error stop 9
    copy = owner
    select type (part => source%part)
    type is (Child)
        part%numbers = 41
    end select
    if (owner%value() /= 17 .or. copy%value() /= 17) error stop 4
    before = finalized
    deallocate(source%part)
    if (finalized /= before + 1) error stop 5
    before = finalized
    deallocate(owner)
    if (finalized /= before + 1) error stop 6
    if (copy%value() /= 17) error stop 7
    before = finalized
    deallocate(copy)
    if (finalized /= before + 1) error stop 8
end program
