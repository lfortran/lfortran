module class_17_mod
    implicit none
    type :: type_pass
        integer :: value
        contains

        procedure :: set_value_pass
        generic :: set_value => set_value_pass
    end type

    type :: type_no_pass
        integer :: value
        contains

        procedure, nopass :: set_value_no_pass, set_value_no_pass_no_obj
        generic :: set_value => set_value_no_pass, set_value_no_pass_no_obj
    end type

    contains

    subroutine set_value_pass(this, value)
        class(type_pass), intent(inout) :: this
        integer, intent(in) :: value
        this%value = 2 * value
    end subroutine

    subroutine set_value_no_pass(obj, value)
        class(type_no_pass), intent(inout) :: obj
        integer, intent(in) :: value
        obj%value = value
    end subroutine

    subroutine set_value_no_pass_no_obj(value)
        integer, intent(inout) :: value
        value = 25
    end subroutine
end module

program class_17
    use class_17_mod
    implicit none
    integer :: value

    type(type_pass) :: obj_pass
    type(type_no_pass) :: obj_no_pass

    obj_pass%value = 42

    ! below tests show different calls to the same
    ! GenericProcedure (actually the same StructMethodDeclaration)
    ! case 1. passed as argument
    call obj_pass%set_value(45)
    print *, "obj_pass%value: ", obj_pass%value
    if (obj_pass%value /= 90) error stop
    ! case 2. passed as kwarg
    call obj_pass%set_value(value=50)
    print *, "obj_pass%value: ", obj_pass%value
    if (obj_pass%value /= 100) error stop


    obj_no_pass%value = 42
    ! below tests show different calls to the same
    ! GenericProcedure (actually the same StructMethodDeclaration)
    ! case 1. both are arguments
    call obj_no_pass%set_value(obj_no_pass, 5)
    print *, "obj_no_pass%value: ", obj_no_pass%value
    if (obj_no_pass%value /= 5) error stop

    ! case 2. first is argument, second is kwarg
    call obj_no_pass%set_value(obj_no_pass, value=10)
    print *, "obj_no_pass%value: ", obj_no_pass%value
    if (obj_no_pass%value /= 10) error stop

    ! case 3. both are kwargs
    call obj_no_pass%set_value(obj=obj_no_pass, value=11)
    print *, "obj_no_pass%value: ", obj_no_pass%value
    if (obj_no_pass%value /= 11) error stop

    ! case 4. both are kwargs, but position interchanged
    call obj_no_pass%set_value(value=64, obj=obj_no_pass)
    print *, "obj_no_pass%value: ", obj_no_pass%value
    if (obj_no_pass%value /= 64) error stop


    value = 10
    ! below tests show different calls to the same
    ! GenericProcedure (actually the same StructMethodDeclaration),
    ! which accepts only one argument
    ! case 1. passed as argument
    call obj_no_pass%set_value(value)
    print *, "value: ", value
    if (value /= 25) error stop

    value = 30
    ! case 2. passed as kwarg
    call obj_no_pass%set_value(value=value)
    print *, "value: ", value
    if (value /= 25) error stop
end program class_17
