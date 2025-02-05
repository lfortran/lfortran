module derived_types_13_module_01
    implicit none
    type t
        integer :: i
        integer, allocatable :: xl(:) 
    end type t
contains
    function new_value() result(p)
        type(t) :: p
        p%i = 123
    end function new_value

    subroutine alloc(x)
        integer, allocatable, intent(out) :: x(:)
        allocate(x(5))
    end subroutine alloc
end module derived_types_13_module_01

program derived_types_13
    use derived_types_13_module_01
    implicit none

    type(t) :: x
    x = new_value()
    print *, x % i
    if (x%i /= 123) error stop
    call alloc(x % xl)
    print *, size(x % xl)
    if (size(x%xl) /= 5) error stop
end program derived_types_13
