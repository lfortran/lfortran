module traits_runtime_05_provider_m
    use iso_c_binding, only: c_ptr, c_loc, c_associated
    use traits_runtime_05_contracts_m, only: ICombined, Box, expected_address
    implicit none
    private
    public :: acquire, update
    type(Box), target, save :: storage
    implements ICombined :: Box
        procedure, nopass :: label => box_label
        procedure, pass :: double_value => box_double
        procedure, pass :: value => box_value
    end implements
contains
    integer function box_value(self)
        type(Box), target, intent(in) :: self
        type(c_ptr) :: actual_address
        actual_address = c_loc(self%payload)
        if (.not. c_associated(actual_address, expected_address)) error stop 501
        box_value = self%payload
    end function
    integer function box_double(self)
        type(Box), target, intent(in) :: self
        type(c_ptr) :: actual_address
        actual_address = c_loc(self%payload)
        if (.not. c_associated(actual_address, expected_address)) error stop 502
        box_double = 2 * self%payload
    end function
    integer function box_label()
        box_label = 101
    end function
    subroutine acquire(n, object)
        integer, intent(in) :: n
        class(ICombined), pointer, intent(out) :: object
        storage%payload = n
        expected_address = c_loc(storage%payload)
        object => storage
    end subroutine
    subroutine update(n)
        integer, intent(in) :: n
        storage%payload = n
    end subroutine
end module

subroutine r3_acquire(n, object)
    use traits_runtime_05_contracts_m, only: ICombined
    use traits_runtime_05_provider_m, only: acquire
    implicit none
    integer, intent(in) :: n
    class(ICombined), pointer, intent(out) :: object
    call acquire(n, object)
end subroutine

subroutine r3_update(n)
    use traits_runtime_05_provider_m, only: update
    implicit none
    integer, intent(in) :: n
    call update(n)
end subroutine
