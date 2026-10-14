! Structure constructors of derived types declared in procedures of a
! template, instantiated in a module compiled separately from its user.

program template_local_type_02
    use template_local_type_02_inst, only: f3, h3
    implicit none
    print *, f3(), h3()
    if (f3() /= 6) error stop
    if (h3() /= 12) error stop
end program
