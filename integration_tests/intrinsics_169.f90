program intrinsics_169
    implicit none

    integer(4) :: a
    a =  -huge(-1) - 1

    print *, mod(a, 10)

    if (mod(a, 10) /= -8) error stop

end program
