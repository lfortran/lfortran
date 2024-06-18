program intrinsics_259
    
    implicit none
    character(5) :: hello = "hello"
    character(5) :: world = "world"
    character(8) :: lfortran = "lfortran"
    character(7) :: fortran = "fortran"
    character(5) :: sym = "#s@ym"
    character(4) :: sym2 = "s_y!"

    logical, parameter :: l1 = lgt("hello", "world")
    logical, parameter :: l2 = lge("lfortran", "fortran")
    logical, parameter :: l3 = llt("#s@ym", "world")
    logical, parameter :: l4 = lle("hello", "s_y!")

    logical, parameter :: ar1(3) = lgt(["hello", "world", "#s@ym"], ["world", "hello", "s_y!0"])
    logical, parameter :: ar2(3) = lgt(["lfortran", "gfortran", "fort_ran"], ["world", "hello", "s_y!0"])
    logical, parameter :: ar3(3) = lge(["lfortran", "gfortran", "fort_ran"], ["hello", "world", "#s@ym"])
    logical, parameter :: ar4(3) = llt(["hello", "world", "#s@ym"], ["world", "hello", "s_y!0"])

    character(5) :: arr1(3)
    character(5) :: arr2(3)
    character(8) :: arr3(3)
    character(5) :: arr4(3)
    logical :: res(3)

    arr1 = ["hello", "world", "#s@ym"]
    arr2 = ["world", "hello", "s_y!0"]
    arr3 = ["lfortran", "gfortran", "fort_ran"]
    arr4 = ["world", "hello", "s_y!0"]

    print *, l1
    if (l1 .neqv. .false.) error stop
    print *, l2
    if (l2 .neqv. .true.) error stop
    print *, l3
    if (l3 .neqv. .true.) error stop
    print *, l4
    if (l4 .neqv. .true.) error stop

    print *, ar1
    ! if (all(ar1 .neqv. [.false., .true., .false.])) error stop ! does not work yet
    print *, ar2
    ! if (all(ar2 .neqv. [.false., .false., .false.])) error stop
    print *, ar3
    ! if (all(ar3 .neqv. [.true., .false., .true.])) error stop
    print *, ar4
    ! if (all(ar4 .neqv. [.true., .false., .true.])) error stop

    print *, lgt(hello, world)
    if (lgt(hello, world) .neqv. .false.) error stop
    print *, lge(lfortran, fortran)
    if (lge(lfortran, fortran) .neqv. .true.) error stop
    print *, llt(sym, world)
    if (llt(sym, world) .neqv. .true.) error stop
    print *, lle(hello, sym2)
    if (lle(hello, sym2) .neqv. .true.) error stop

    print *, lgt(arr1, arr2)
    if (all(lgt(arr1, arr2) .neqv. [.true., .true., .true.])) error stop
    print *, lge(arr2, arr3)
    if (all(lge(arr2, arr3) .neqv. [.true., .true., .true.])) error stop
    print *, llt(arr3, arr4)
    if (all(llt(arr3, arr4) .neqv. [.true., .true., .true.])) error stop
    print *, lle(arr4, arr1)
    if (all(lle(arr4, arr1) .neqv. [.true., .true., .true.])) error stop

    res = lgt(arr1, arr2)
    if (all(res .neqv. [.true., .true., .true.])) error stop
    res = lge(arr2, arr3)
    if (all(res .neqv. [.true., .true., .true.])) error stop
    res = llt(arr3, arr4)
    if (all(res .neqv. [.true., .true., .true.])) error stop
    res = lle(arr4, arr1)
    if (all(res .neqv. [.true., .true., .true.])) error stop

end program