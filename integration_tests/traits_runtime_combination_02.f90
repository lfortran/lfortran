module traits_runtime_combination_02_m
    implicit none
    abstract interface :: IValue
        integer function value()
        end function
    end interface
    abstract interface :: ILabel
        integer function label()
        end function
    end interface
    abstract interface :: IExtra
        integer function extra()
        end function
    end interface
    abstract interface, extends(ILabel + IValue) :: IChild
    end interface
    type :: Value
        integer :: n = 0
        integer, allocatable :: data(:)
    contains
        final :: finalize_value
    end type
    implements IChild + IExtra :: Value
        procedure, pass :: value => read_value
        procedure, nopass :: label => read_label
        procedure, pass :: extra => read_extra
    end implements
    type(Value) :: source
    integer :: finals = 0, final_sum = 0
    abstract interface
        function factory_signature(n) result(object)
            import :: IValue, ILabel
            integer, intent(in) :: n
            class(ILabel + IValue), allocatable :: object
        end function
    end interface
contains
    integer function read_value(self)
        class(Value), intent(in) :: self
        read_value = self%n
        if (allocated(self%data)) then
            if (self%data(1) /= self%n .or. self%data(2) /= self%n+1) error stop 1401
        end if
    end function
    integer function read_label()
        read_label = 101
    end function
    integer function read_extra(self)
        type(Value), intent(in) :: self
        read_extra = 3*self%n
    end function
    subroutine finalize_value(self)
        type(Value), intent(inout) :: self
        finals = finals + 1
        final_sum = final_sum + self%n
        self%n = -777
        if (allocated(self%data)) self%data = -888
    end subroutine
    function make_rich(n) result(object)
        integer, intent(in) :: n
        class(IExtra + IValue + ILabel), allocatable :: object
        if (.not. allocated(source%data)) allocate(source%data(2))
        source%n = n
        source%data = [n, n+1]
        allocate(object, source=source)
    end function
    function relay(n) result(object)
        integer, intent(in) :: n
        class(ILabel + IValue), allocatable :: object
        object = make_rich(n)
    end function
    class(IValue + ILabel) function prefixed(n) result(object)
        integer, intent(in) :: n
        allocatable :: object
        object = make_rich(n)
    end function
    subroutine observe(object, n)
        class(IValue + ILabel) :: object
        intent(in) :: object
        integer, intent(in) :: n
        if (object%value() /= n .or. object%label() /= 101) error stop 1402
    end subroutine
    integer function borrow_result(object)
        class(ILabel + IValue), intent(in) :: object
        borrow_result = object%value()
    end function
    subroutine saved_owner(clear)
        logical, intent(in) :: clear
        class(IValue + ILabel), allocatable, save :: object
        if (clear) then
            if (object%value() /= 47) error stop 1403
            deallocate(object)
        else
            object = make_rich(47)
        end if
    end subroutine
end module

program traits_runtime_combination_02
    use traits_runtime_combination_02_m
    implicit none
    class(IValue + ILabel), allocatable :: owner
    class(IValue), allocatable :: member
    procedure(factory_signature), pointer :: selected_factory => null()
    integer :: i, values(4)

    call observe(make_rich(17), 17)
    if (finals /= 1 .or. final_sum /= 17) error stop 1410
    owner = make_rich(23)
    source%n = 88
    source%data = [88, 89]
    call observe(owner, 23)
    if (finals /= 2 .or. final_sum /= 40) error stop 1411
    deallocate(owner)
    if (finals /= 3 .or. final_sum /= 63) error stop 1412
    owner = relay(31)
    call observe(owner, 31)
    if (finals /= 5 .or. final_sum /= 125) error stop 1413
    deallocate(owner)
    if (finals /= 6 .or. final_sum /= 156) error stop 1414
    allocate(owner, source=make_rich(37))
    call observe(owner, 37)
    if (finals /= 7 .or. final_sum /= 193) error stop 1415
    deallocate(owner)
    if (finals /= 8 .or. final_sum /= 230) error stop 1416
    values = [(borrow_result(make_rich(i)), i=1,4)]
    if (any(values /= [1,2,3,4])) error stop 1417
    if (finals /= 12 .or. final_sum /= 240) error stop 1418
    if (borrow_result(make_rich(41)) == 41) then
        if (finals /= 12) error stop 1419
    else
        error stop 1420
    end if
    if (finals /= 13 .or. final_sum /= 281) error stop 1421
    allocate(Value :: owner)
    call observe(owner, 0)
    member = owner
    if (member%value() /= 0) error stop 1422
    deallocate(owner, member)
    if (finals /= 15 .or. final_sum /= 281) error stop 1423
    call saved_owner(.false.)
    if (finals /= 16 .or. final_sum /= 328) error stop 1424
    call saved_owner(.true.)
    if (finals /= 17 .or. final_sum /= 375) error stop 1425
    block
        class(IValue + ILabel), allocatable :: local
        local = make_rich(53)
        call observe(local, 53)
    end block
    if (finals /= 19 .or. final_sum /= 481) error stop 1426
    call observe(prefixed(59), 59)
    if (finals /= 21 .or. final_sum /= 599) error stop 1427
    selected_factory => prefixed
    call observe(selected_factory(61), 61)
    if (finals /= 23 .or. final_sum /= 721) error stop 1428
    nullify(selected_factory)
    deallocate(source%data)
end program
