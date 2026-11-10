! A local derived type with the same name as a module derived type that is
! not imported (use, only:) (#5874)
module derived_types_257_mod
    type :: my_type
        integer :: i
    end type

    type(my_type) :: my_var
end module

program derived_types_257
    use derived_types_257_mod, only: my_var

    type :: my_type
        real :: r
    end type

    type(my_type) :: my_second_var

    my_var%i = 1
    print *, my_var%i
    if (my_var%i /= 1) error stop

    my_second_var%r = 3.14
    print *, my_second_var%r
    if (abs(my_second_var%r - 3.14) > 1e-6) error stop
end program
