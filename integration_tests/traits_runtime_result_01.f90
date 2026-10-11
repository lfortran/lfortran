module traits_runtime_result_01_m
    implicit none
    abstract interface :: IValue
        function value() result(r)
            integer :: r
        end function
    end interface
    type :: ValueA
        integer, allocatable :: data(:)
    contains
        final :: finalize_a
    end type
    implements IValue :: ValueA
        procedure, pass :: value => value_a
    end implements
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
        if (.not. allocated(self%data)) error stop 901
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
    subroutine observe(object, expected)
        class(IValue), intent(in) :: object
        integer, intent(in) :: expected
        if (object%value() /= expected) error stop 902
    end subroutine
end module

program traits_runtime_result_01
    use traits_runtime_result_01_m
    implicit none
    class(IValue), allocatable :: owner

    call observe(make_a(17), 17)
    if (finals /= 1 .or. finalized_values /= 17) error stop 903
    owner = make_a(23)
    source%data = [88, 89]
    call observe(owner, 23)
    if (finals /= 2 .or. finalized_values /= 40) error stop 904
    deallocate(owner)
    if (finals /= 3 .or. finalized_values /= 63) error stop 905
    owner = relay_a(31)
    call observe(owner, 31)
    if (finals /= 5 .or. finalized_values /= 125) error stop 906
    deallocate(owner)
    if (finals /= 6 .or. finalized_values /= 156) error stop 907
    allocate(owner, source=make_a(37))
    call observe(owner, 37)
    if (finals /= 7 .or. finalized_values /= 193) error stop 908
    deallocate(owner)
    if (finals /= 8 .or. finalized_values /= 230) error stop 909
    deallocate(source%data)
end program
