program binop_02
    real(8) :: v
    integer(2), parameter :: exp_2 = 2
    integer(4), parameter :: exp_3 = 3
    integer(8), parameter :: exp_4 = 4
    
    v = 3.733689483637092187962025491287931799888610839_8

    print "(es23.16)", v*v
    print "(es23.16)", v**exp_2
    if(v*v /= v**exp_2) error stop
    
    print "(es23.16)", v*v*v
    print "(es23.16)", v**exp_3
    if(v*v*v /= v**exp_3) error stop
    
    print "(es23.16)", v*v*v*v
    print "(es23.16)", v**exp_4
    if(v*v*v*v /= v**exp_4) error stop

end program binop_02