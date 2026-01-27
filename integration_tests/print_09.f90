program print_09
    implicit none
    real :: x, y, z, w
    real(8) :: a, b, c
    
    ! Test various float values with print *
    x = 0.14
    print *, x
    if (abs(x - 0.14) > 1.0e-6) error stop "x value incorrect"
    
    y = 123.456
    print *, y
    if (abs(y - 123.456) > 1.0e-3) error stop "y value incorrect"
    
    z = 0.001
    print *, z
    if (abs(z - 0.001) > 1.0e-6) error stop "z value incorrect"
    
    w = 1.5e-5
    print *, w
    if (abs(w - 1.5e-5) > 1.0e-8) error stop "w value incorrect"
    
    ! Test double precision values
    a = 0.14d0
    print *, a
    if (abs(a - 0.14d0) > 1.0d-10) error stop "a value incorrect"
    
    b = 456.789d0
    print *, b
    if (abs(b - 456.789d0) > 1.0d-6) error stop "b value incorrect"
    
    c = 0.0001d0
    print *, c
    if (abs(c - 0.0001d0) > 1.0d-10) error stop "c value incorrect"
    
    ! Test write(*,*)
    print *, "Testing write(*,*) format:"
    write(*,*) x
    write(*,*) y
    write(*,*) a, b
    
    print *, "All tests passed"
end program print_09
