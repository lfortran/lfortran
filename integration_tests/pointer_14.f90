program pointer_14
    implicit none

    type :: my_type
        integer :: x
    end type my_type

    type(my_type), target :: my_var
    type(my_type), pointer :: p => my_var

    my_var%x = 123
    if (.not. associated(p)) error stop
    if (p%x /= 123) error stop

    p%x = 321
    if (my_var%x /= 321) error stop
end program pointer_14
