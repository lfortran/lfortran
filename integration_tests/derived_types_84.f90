program derived_types_84
    use derived_types_84_module
    implicit none
    type(Person) :: p1
    p1%age  = 25
    call get_age(p1)
    print *, p1%age
    if (p1%age /= 30) error stop
end program derived_types_84
