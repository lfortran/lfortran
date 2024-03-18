program intrinsics_188
        
    character(5) :: hello = "hello"
    character(5) :: world = "world"
    character(8) :: lfortran = "lfortran"
    character(7) :: fortran = "fortran"
    character(5) :: sym = "#s@ym"
    character(4) :: sym2 = "s_y!"

    print*, lgt("hello", "world")
    if (lgt("hello", "world") .neqv. .false.) error stop 
    print*, lgt("hello", "hello")
    if (lgt("hello", "hello") .neqv. .false.) error stop
    print*, lgt("lfortran", "fortran")
    if (lgt("lfortran", "fortran") .neqv. .true.) error stop
    print*, lgt("#s@ym", "s_y!")
    if (lgt("#s@ym", "s_y!") .neqv. .false.) error stop
    print*, lgt("hello", "s_y!")
    if (lgt("hello", "s_y!") .neqv. .false.) error stop

    print*, lge("hello", "world")
    if (lge("hello", "world") .neqv. .false.) error stop 
    print*, lge("hello", "hello")
    if (lge("hello", "hello") .neqv. .true.) error stop
    print*, lge("lfortran", "fortran")
    if (lge("lfortran", "fortran") .neqv. .true.) error stop
    print*, lge("#s@ym", "s_y!")
    if (lge("#s@ym", "s_y!") .neqv. .false.) error stop
    print*, lge("hello", "s_y!")
    if (lge("hello", "s_y!") .neqv. .false.) error stop

    print*, llt("hello", "world")
    if (llt("hello", "world") .neqv. .true.) error stop 
    print*, llt("hello", "hello")
    if (llt("hello", "hello") .neqv. .false.) error stop
    print*, llt("lfortran", "fortran")
    if (llt("lfortran", "fortran") .neqv. .false.) error stop
    print*, llt("#s@ym", "s_y!")
    if (llt("#s@ym", "s_y!") .neqv. .true.) error stop
    print*, llt("hello", "s_y!")
    if (llt("hello", "s_y!") .neqv. .true.) error stop

    print*, lle("hello", "world")
    if (lle("hello", "world") .neqv. .true.) error stop 
    print*, lle("hello", "hello")
    if (lle("hello", "hello") .neqv. .true.) error stop
    print*, lle("lfortran", "fortran")
    if (lle("lfortran", "fortran") .neqv. .false.) error stop
    print*, lle("#s@ym", "s_y!")
    if (lle("#s@ym", "s_y!") .neqv. .true.) error stop
    print*, lle("hello", "s_y!")
    if (lle("hello", "s_y!") .neqv. .true.) error stop

    print*, lgt(hello, world)
    if (lgt(hello, world) .neqv. .false.) error stop
    print*, lgt(hello, hello)
    if (lgt(hello, hello) .neqv. .false.) error stop
    print*, lgt(lfortran, fortran)
    if (lgt(lfortran, fortran) .neqv. .true.) error stop
    print*, lgt(sym, sym2)
    if (lgt(sym, sym2) .neqv. .false.) error stop
    print*, lgt(hello, sym2)
    if (lgt(hello, sym2) .neqv. .false.) error stop

    print*, lge(hello, world)
    if (lge(hello, world) .neqv. .false.) error stop
    print*, lge(hello, hello)
    if (lge(hello, hello) .neqv. .true.) error stop
    print*, lge(lfortran, fortran)
    if (lge(lfortran, fortran) .neqv. .true.) error stop
    print*, lge(sym, sym2)
    if (lge(sym, sym2) .neqv. .false.) error stop
    print*, lge(hello, sym2)
    if (lge(hello, sym2) .neqv. .false.) error stop

    print*, llt(hello, world)
    if (llt(hello, world) .neqv. .true.) error stop
    print*, llt(hello, hello)
    if (llt(hello, hello) .neqv. .false.) error stop
    print*, llt(lfortran, fortran)
    if (llt(lfortran, fortran) .neqv. .false.) error stop
    print*, llt(sym, sym2)
    if (llt(sym, sym2) .neqv. .true.) error stop
    print*, llt(hello, sym2)
    if (llt(hello, sym2) .neqv. .true.) error stop

    print*, lle(hello, world)
    if (lle(hello, world) .neqv. .true.) error stop
    print*, lle(hello, hello)
    if (lle(hello, hello) .neqv. .true.) error stop
    print*, lle(lfortran, fortran)
    if (lle(lfortran, fortran) .neqv. .false.) error stop
    print*, lle(sym, sym2)
    if (lle(sym, sym2) .neqv. .true.) error stop
    print*, lle(hello, sym2)
    if (lle(hello, sym2) .neqv. .true.) error stop
    
end program