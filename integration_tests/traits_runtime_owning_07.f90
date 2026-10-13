module traits_runtime_owning_07_m
    implicit none
    integer :: finals = 0, finalized_values = 0
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: Part
        integer :: n = 7
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
        finalized_values = finalized_values + self%n
    end subroutine
    function read_value(self) result(r)
        class(Envelope), intent(in) :: self
        integer :: r, i
        r = sum(self%parts%n)
        do i = 1, size(self%parts)
            r = r + sum(self%parts(i)%data)
        end do
    end function
end module

program traits_runtime_owning_07
    use traits_runtime_owning_07_m
    implicit none
    type(Envelope) :: source
    class(IValue), allocatable :: owner, copy
    integer :: i

    allocate(source%parts(2))
    source%parts%n = 17
    do i = 1, 2
        allocate(source%parts(i)%data(1))
        source%parts(i)%data = 5
    end do
    allocate(owner, source=source)
    if (finals /= 0 .or. owner%value() /= 44) error stop 1
    copy = owner
    if (finals /= 0 .or. copy%value() /= 44) error stop 2
    source%parts%n = 23
    source%parts(1)%data = 99
    if (owner%value() /= 44 .or. copy%value() /= 44) error stop 3
    owner = owner
    if (finals /= 2 .or. finalized_values /= 34) error stop 4
    if (owner%value() /= 44) error stop 5
    deallocate(owner, copy)
    if (finals /= 6 .or. finalized_values /= 102) error stop 6
    deallocate(source%parts)
    if (finals /= 8 .or. finalized_values /= 148) error stop 7
end program
