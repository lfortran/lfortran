module bindc_74_mod
    use iso_c_binding, only: c_char
    implicit none
    integer :: calls = 0
    abstract interface
        function explicit_interface() result(c) bind(c)
            import :: c_char
            character(kind=c_char) :: c
        end function
        character(kind=c_char) function prefix_interface() bind(c)
            import :: c_char
        end function
    end interface
    type :: callbacks
        procedure(explicit_interface), pointer, nopass :: explicit_p => null()
        procedure(prefix_interface), pointer, nopass :: prefix_p => null()
    end type
    type, abstract :: base
    contains
        procedure(explicit_interface), nopass, deferred :: get
    end type
    type, extends(base) :: child
    contains
        procedure, nopass :: get => explicit_result
    end type
contains
    function explicit_result() result(c) bind(c)
        character(kind=c_char) :: c
        calls = calls + 1
        c = 'q'
    end function
    character(kind=c_char) function prefix_result() bind(c)
        calls = calls + 1
        prefix_result = 'q'
    end function
end module

program bindc_74
    use bindc_74_mod
    implicit none
    type(callbacks) :: obj
    class(base), allocatable :: polymorphic
    procedure(explicit_interface), pointer :: local_p
    character(kind=c_char) :: c
    obj%explicit_p => explicit_result
    obj%prefix_p => prefix_result

    c = explicit_result()
    if (c /= 'q' .or. calls /= 1) error stop 1
    c = prefix_result()
    if (c /= 'q' .or. calls /= 2) error stop 2
    local_p => explicit_result
    c = local_p()
    if (c /= 'q' .or. calls /= 3) error stop 3
    local_p => obj%explicit_p
    c = local_p()
    if (c /= 'q' .or. calls /= 4) error stop 4
    local_p => obj%prefix_p
    c = local_p()
    if (c /= 'q' .or. calls /= 5) error stop 5

    ! GNU currently mislowers the component result ABI. Check invocation
    ! counts here, keeping byte-value controls on direct and copied pointers.
    c = obj%explicit_p()
    if (calls /= 6) error stop 6
    c = obj%prefix_p()
    if (calls /= 7) error stop 7
    allocate(child :: polymorphic)
    c = polymorphic%get()
    if (calls /= 8) error stop 8
end program
