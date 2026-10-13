module traits_runtime_owning_11_m
    implicit none
    integer :: finals = 0, final_values = 0
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: Part
        integer :: n = 17
        integer, allocatable :: data(:)
    contains
        final :: finish
    end type
    type :: Envelope
        type(Part), allocatable :: parts(:)
    end type
    implements IValue :: Envelope
        procedure, pass :: value => read_value
    end implements
contains
    impure elemental subroutine finish(self)
        type(Part), intent(inout) :: self
        finals = finals + 1
        final_values = final_values + self%n
        if (allocated(self%data)) then
            if (any(self%data /= 5)) error stop 10
        end if
    end subroutine
    function read_value(self) result(r)
        class(Envelope), intent(in) :: self
        integer :: r
        r = -1
        if (allocated(self%parts)) r = sum(self%parts%n)
    end function
end module

program traits_runtime_owning_11
    use traits_runtime_owning_11_m
    implicit none
    type(Envelope) :: source, empty
    class(IValue), allocatable :: owner, copy
    integer :: i
    allocate(source%parts(2))
    do i = 1, 2
        allocate(source%parts(i)%data(3))
        source%parts(i)%data = 5
    end do
    allocate(owner, source=source)
    allocate(copy, source=empty)
    if (finals /= 0 .or. owner%value() /= 34) error stop 1
    owner = empty
    if (finals /= 2 .or. final_values /= 34) error stop 2
    if (owner%value() /= -1) error stop 3
    owner = owner
    if (finals /= 2 .or. owner%value() /= -1) error stop 4
    owner = source
    if (finals /= 2 .or. owner%value() /= 34) error stop 5
    owner = copy
    if (finals /= 4 .or. final_values /= 68) error stop 6
    if (owner%value() /= -1) error stop 7
    deallocate(owner, copy, source%parts)
    if (finals /= 6 .or. final_values /= 102) error stop 8
end program
