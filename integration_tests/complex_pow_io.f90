program complex_pow_io
    implicit none
    complex :: z32
    complex(8) :: z64

    z32 = (0.0, 0.0)
    z64 = (0.d0, 0.d0)
    
    print *, z32**2
    print *, z64**2

    write(*, "(a)", advance="no") "A"
    write(*, "(a)", advance="no") " "
    write(*, "(a)") "B"
end program