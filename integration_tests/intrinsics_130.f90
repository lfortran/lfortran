program intrinsics_128
    print*, ishftc(10, 2)
    if (.not. ishftc(10, 2) == 40) error stop

end program
