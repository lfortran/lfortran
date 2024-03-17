program intrinsics_188
        
    character(5) :: hello = "hello"
    character(5) :: world = "world"
    character(8) :: lfortran = "lfortran"
    character(7) :: fortran = "fortran"

    print*, lgt("hello", "world")
    if (lgt("hello", "world") .neqv. .false.) error stop 
    print*, lgt("hello", "hello")
    if (lgt("hello", "hello") .neqv. .false.) error stop
    print*, lgt("lfortran", "fortran")
    if (lgt("lfortran", "fortran") .neqv. .true.) error stop

    print*, lge("hello", "world")
    if (lge("hello", "world") .neqv. .false.) error stop 
    print*, lge("hello", "hello")
    if (lge("hello", "hello") .neqv. .true.) error stop
    print*, lge("lfortran", "fortran")
    if (lge("lfortran", "fortran") .neqv. .true.) error stop

    print*, llt("hello", "world")
    if (llt("hello", "world") .neqv. .true.) error stop 
    print*, llt("hello", "hello")
    if (llt("hello", "hello") .neqv. .false.) error stop
    print*, llt("lfortran", "fortran")
    if (llt("lfortran", "fortran") .neqv. .false.) error stop

    print*, lle("hello", "world")
    if (lle("hello", "world") .neqv. .true.) error stop 
    print*, lle("hello", "hello")
    if (lle("hello", "hello") .neqv. .true.) error stop
    print*, lle("lfortran", "fortran")
    if (lle("lfortran", "fortran") .neqv. .false.) error stop

    print*, lgt(hello, world)
    if (lgt(hello, world) .neqv. .false.) error stop
    print*, lgt(hello, hello)
    if (lgt(hello, hello) .neqv. .false.) error stop
    print*, lgt(lfortran, fortran)
    if (lgt(lfortran, fortran) .neqv. .true.) error stop

    print*, lge(hello, world)
    if (lge(hello, world) .neqv. .false.) error stop
    print*, lge(hello, hello)
    if (lge(hello, hello) .neqv. .true.) error stop
    print*, lge(lfortran, fortran)
    if (lge(lfortran, fortran) .neqv. .true.) error stop

    print*, llt(hello, world)
    if (llt(hello, world) .neqv. .true.) error stop
    print*, llt(hello, hello)
    if (llt(hello, hello) .neqv. .false.) error stop
    print*, llt(lfortran, fortran)
    if (llt(lfortran, fortran) .neqv. .false.) error stop

    print*, lle(hello, world)
    if (lle(hello, world) .neqv. .true.) error stop
    print*, lle(hello, hello)
    if (lle(hello, hello) .neqv. .true.) error stop
    print*, lle(lfortran, fortran)
    if (lle(lfortran, fortran) .neqv. .false.) error stop
    
end