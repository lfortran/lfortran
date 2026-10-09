module traits_runtime_result_oracle_m
    implicit none
    type, abstract :: IValue
    contains
        procedure(value_interface), deferred :: value
    end type
    abstract interface
        integer function value_interface(self)
            import IValue
            class(IValue), intent(in) :: self
        end function
    end interface
    type, extends(IValue) :: ValueA
        integer, allocatable :: data(:)
    contains
        procedure :: value => value_a
        final :: finalize_a
    end type
    type(ValueA) :: source
    integer :: finals = 0, finalized_values = 0
contains
    integer function value_a(self)
        class(ValueA), intent(in) :: self
        value_a = self%data(1)
    end function
    subroutine finalize_a(self)
        type(ValueA), intent(inout) :: self
        finals = finals + 1
        if (.not. allocated(self%data)) error stop 1
        finalized_values = finalized_values + self%data(1)
        self%data = -777
    end subroutine
    function make_a(n) result(object)
        integer, intent(in) :: n
        class(IValue), allocatable :: object
        if (.not. allocated(source%data)) allocate(source%data(2))
        source%data = [n, n + 1]
        allocate(object, source=source)
    end function
    function relay_a(n) result(object)
        integer, intent(in) :: n
        class(IValue), allocatable :: object
        object = make_a(n)
    end function
end module

program traits_runtime_result_01_oracle
    use traits_runtime_result_oracle_m
    implicit none
    class(IValue), allocatable :: owner

    ! GFortran 16 omits FINAL for a directly borrowed function result.
    ! That normative lifetime is tested unchanged in traits_runtime_result_01.
    ! This standard control covers its correctly implemented copy boundaries.
    owner = make_a(23)
    source%data = [88, 89]
    if (owner%value() /= 23) error stop 2
    if (finals /= 1 .or. finalized_values /= 23) error stop 3
    deallocate(owner)
    if (finals /= 2 .or. finalized_values /= 46) error stop 4
    owner = relay_a(31)
    if (owner%value() /= 31) error stop 5
    if (finals /= 4 .or. finalized_values /= 108) error stop 6
    deallocate(owner)
    if (finals /= 5 .or. finalized_values /= 139) error stop 7
    allocate(owner, source=make_a(37))
    if (owner%value() /= 37) error stop 8
    if (finals /= 6 .or. finalized_values /= 176) error stop 9
    deallocate(owner)
    if (finals /= 7 .or. finalized_values /= 213) error stop 10
    deallocate(source%data)
end program
