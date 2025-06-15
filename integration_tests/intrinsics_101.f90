program intrinsics_101
    implicit none
    character(len=5) :: s1
    character(len=10) :: s2
    s1 = "242"
    s2 = repeat(s1, 2)
    print *, repeat("hello", 0)
    if (repeat("hello", 0) /= "") error stop 1
    print *, repeat("hello", 2)
    if (repeat("hello", 2) /= "hellohello") error stop 2
    print *, '-', s2, '-'
    if (s2 /= "242  242  ") error stop 3
end program
