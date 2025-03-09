program binop_02
    real(8) :: v
    integer(2), parameter :: exp_2 = 2
    integer(4), parameter :: exp_3 = 3
    integer(8), parameter :: exp_4 = 4
    logical :: fast = .false.

    if (index(compiler_options(), '--fast') /= 0) then
        print *, '--fast is specified, disabling some tests'
        fast = .true.
    end if

    v = 3.733689483637092187962025491287931799888610839_8

    print "(es23.16)", v*v
    print "(es23.16)", v**exp_2
    if(v*v /= v**exp_2) error stop

    print "(es23.16)", v*v*v
    print "(es23.16)", v**exp_3
    if (.not. fast) then
        if(v*v*v /= v**exp_3) error stop
    end if

    print "(es23.16)", v*v*v*v
    print "(es23.16)", v**exp_4
    if (.not. fast) then
        if(v*v*v*v /= v**exp_4) error stop
    end if

end program binop_02
