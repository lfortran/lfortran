! The binding label of an external procedure declared with a BIND(C)
! interface is its own name, or the NAME= of its own BIND(C), never the
! binding label of the interface
module procedure_56_m
    use iso_c_binding, only: c_double
    implicit none
    interface
        real(c_double) function cfun(x) bind(c, name="procedure_56_triple")
            import :: c_double
            real(c_double), value :: x
        end function
    end interface
end module

real(c_double) function procedure_56_triple_impl(x) &
        bind(c, name="procedure_56_triple")
    use iso_c_binding, only: c_double
    real(c_double), value :: x
    procedure_56_triple_impl = 3*x
end function

real(c_double) function procedure_56_twice(x) bind(c)
    use iso_c_binding, only: c_double
    real(c_double), value :: x
    procedure_56_twice = 2*x
end function

real(c_double) function procedure_56_five_impl(x) &
        bind(c, name="procedure_56_five")
    use iso_c_binding, only: c_double
    real(c_double), value :: x
    procedure_56_five_impl = 5*x
end function

program procedure_56
    use procedure_56_m, only: cfun
    implicit none
    procedure(cfun) :: procedure_56_twice
    procedure(cfun), bind(c, name="procedure_56_five") :: five
    double precision :: d
    d = cfun(1.0d0)
    print *, d
    if (abs(d - 3.0d0) > 1d-12) error stop
    d = procedure_56_twice(1.0d0)
    print *, d
    if (abs(d - 2.0d0) > 1d-12) error stop
    d = five(1.0d0)
    print *, d
    if (abs(d - 5.0d0) > 1d-12) error stop
end program
