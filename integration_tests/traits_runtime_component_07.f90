module traits_runtime_component_07_m
    implicit none
    integer :: calls = 0, finals = 0, final_sum = 0
    abstract interface :: IValue
        integer function value()
        end function
        subroutine fetch(n)
            integer, intent(out) :: n
        end subroutine
    end interface
    type :: Payload
        integer :: n
    contains
        final :: finish
    end type
    implements IValue :: Payload
        procedure :: value => payload_value
        procedure, pass(self) :: fetch => payload_fetch
    end implements
    type :: Holder
        class(IValue), allocatable :: item
    end type
    type, extends(Holder) :: Child
        integer :: extra = 0
    end type
contains
    integer function payload_value(self)
        type(Payload), intent(in) :: self
        payload_value = self%n
    end function
    integer function next_index()
        calls = calls + 1
        next_index = 1
    end function
    subroutine payload_fetch(n, self)
        integer, intent(out) :: n
        type(Payload), intent(in) :: self
        n = self%n
    end subroutine
    subroutine finish(self)
        type(Payload), intent(inout) :: self
        finals = finals + 1
        final_sum = final_sum + self%n
        self%n = -999
    end subroutine
end module

program traits_runtime_component_07
    use traits_runtime_component_07_m
    implicit none
    type(Payload) :: source
    type(Holder), target :: array(2)
    type(Holder) :: copied(2)
    type(Child) :: extended, extended_copy
    type(Holder), allocatable :: allocated_holder
    class(IValue), pointer :: view
    integer :: n
    source%n = 9
    array(next_index())%item = source
    if (calls /= 1) error stop 1
    n = array(next_index())%item%value()
    if (calls /= 2 .or. n /= 9) error stop 2
    call array(next_index())%item%fetch(n)
    if (calls /= 3 .or. n /= 9) error stop 9
    view => array(1)%item
    if (view%value() /= 9) error stop 3
    nullify(view)
    copied = array
    if (.not. allocated(copied(1)%item)) error stop 4
    if (allocated(copied(2)%item)) error stop 5
    deallocate(array(1)%item)
    if (finals /= 1 .or. copied(1)%item%value() /= 9) error stop 6
    extended%item = source
    extended%extra = 4
    extended_copy = extended
    if (extended_copy%extra /= 4 .or. extended_copy%item%value() /= 9) error stop 7
    deallocate(extended%item, extended_copy%item)
    allocate(allocated_holder)
    allocated_holder%item = source
    deallocate(allocated_holder)
    deallocate(copied(1)%item)
    if (finals /= 5 .or. final_sum /= 45) error stop 8
end program
