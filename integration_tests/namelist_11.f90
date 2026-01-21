program test_null
    implicit none

    ! Test null value syntax: var= (skip assignment)
    integer :: a, b, c
    real :: x, y, z

    namelist /nulltest/ a, b, c, x, y, z

    ! Initialize to known values
    a = 100
    b = 200
    c = 300
    x = 1.5
    y = 2.5
    z = 3.5

    ! Create input file with null values
    ! b= and y= should leave those variables unchanged
    open(unit=10, file='namelist_null.dat', status='replace', form='formatted')
    write(10, '(A)') ' &NULLTEST'
    write(10, '(A)') '  a=1'
    write(10, '(A)') '  b='
    write(10, '(A)') '  c=3'
    write(10, '(A)') '  x=10.5'
    write(10, '(A)') '  y='
    write(10, '(A)') '  z=30.5'
    write(10, '(A)') ' /'
    close(10)

    ! Read namelist with null values
    open(unit=10, file='namelist_null.dat', status='old', form='formatted')
    read(10, nml=nulltest)
    close(10)

    ! Verify a was changed to 1
    if (a /= 1) then
        print *, "Error: a =", a, "expected 1"
        error stop "Null value test failed for a"
    end if

    ! Verify b was NOT changed (null value)
    if (b /= 200) then
        print *, "Error: b =", b, "expected 200 (unchanged)"
        error stop "Null value test failed for b"
    end if

    ! Verify c was changed to 3
    if (c /= 3) then
        print *, "Error: c =", c, "expected 3"
        error stop "Null value test failed for c"
    end if

    ! Verify x was changed to 10.5
    if (abs(x - 10.5) > 1.0e-5) then
        print *, "Error: x =", x, "expected 10.5"
        error stop "Null value test failed for x"
    end if

    ! Verify y was NOT changed (null value)
    if (abs(y - 2.5) > 1.0e-5) then
        print *, "Error: y =", y, "expected 2.5 (unchanged)"
        error stop "Null value test failed for y"
    end if

    ! Verify z was changed to 30.5
    if (abs(z - 30.5) > 1.0e-5) then
        print *, "Error: z =", z, "expected 30.5"
        error stop "Null value test failed for z"
    end if

    print *, "Null value namelist test passed!"

end program test_null
