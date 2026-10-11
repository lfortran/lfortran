module traits_runtime_component_08_m
    implicit none
    logical :: may_finalize = .true.
    integer :: finals = 0
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    type :: Payload
        integer :: n = 11
    contains
        final :: finish
    end type
    implements IValue :: Payload
        procedure :: value => payload_value
    end implements
    type :: Holder
        class(IValue), allocatable :: item
    end type
contains
    integer function payload_value(self)
        type(Payload), intent(in) :: self
        payload_value = self%n
    end function
    subroutine finish(self)
        type(Payload), intent(inout) :: self
        if (.not. may_finalize) error stop 5
        finals = finals + 1
        self%n = -999
    end subroutine
end module

program traits_runtime_component_08
    use traits_runtime_component_08_m
    implicit none
    type(Holder) :: a, b, c
    type(Payload) :: source
    source%n = 29
    allocate(Payload :: a%item)
    if (a%item%value() /= 11) error stop 1
    a%item = source
    allocate(b%item, mold=a%item)
    allocate(c%item, mold=source)
    if (a%item%value() /= 29) error stop 2
    if (b%item%value() /= 11 .or. c%item%value() /= 11) error stop 3
    deallocate(a%item, b%item, c%item)
    if (allocated(a%item) .or. allocated(b%item) .or. allocated(c%item)) error stop 4
    if (finals /= 4) error stop 6
    a%item = source
    may_finalize = .false.
end program
