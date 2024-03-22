program intrinsics_195
    character(len=100) :: line = "     Hello, World!"
    character(len=7) :: line2 = "Okay   "
    character(5) :: line3 = " #o$ "
    character(1) :: line4 = "  "

    print*, len_trim("     Hello, World!")
    if (len_trim("     Hello, World!") /= 18 ) error stop
    print*, len_trim("Okay   ")
    if (len_trim("Okay   ") /= 4 ) error stop
    print*, len_trim(" #o$ ")
    if (len_trim(" #o$ ") /= 4 ) error stop
    print*, len_trim("  ")
    if (len_trim("  ") /= 0 ) error stop

    print*, len_trim(line)
    if (len_trim(line) /= 18 ) error stop
    print*, len_trim(line2)
    if (len_trim(line2) /= 4 ) error stop
    print*, len_trim(line3)
    if (len_trim(line3) /= 4 ) error stop
    print*, len_trim(line4)
    if (len_trim(line4) /= 0 ) error stop
    
end program