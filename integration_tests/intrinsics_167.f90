program intrinsics_167
    integer :: a1 = 5  
    integer :: a2 = 8
    integer :: a3 = -1
    integer :: a4 = -4
    integer :: a5 = -2
    integer :: a6 = -5

    print*, btest(5, 8)
    if (btest(5, 8) .neqv. .false.) error stop
    print*, btest(-1, 5)
    if (btest(-1, 5) .neqv. .true.) error stop
  
    print*, btest(a1, a2)
    if (btest(a1, a2) .neqv. .false.) error stop
    print*, btest(a3, a1)
    if (btest(a3, a1) .neqv. .true.) error stop
    print*, btest(a2, a4)
    if (btest(a2, a4) .neqv. .false.) error stop
    print*, btest(a5, a6)
    if (btest(a5, a6) .neqv. .true.) error stop
  
end program 