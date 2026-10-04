program intrinsics_482
    implicit none
    character(len=4) :: a(2)
    a = repeat("x", 4)
    a = repeat(" ", 4)
    if (any(a /= "    ")) error stop
end program intrinsics_482