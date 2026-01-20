program test_repeat
    implicit none

    ! Test repeat count syntax: n*value
    real :: arr(10)
    integer :: iarr(5)
    integer :: i

    namelist /repeat/ arr, iarr

    ! Initialize to sentinel values
    arr = -999.0
    iarr = -999

    ! Create input file with repeat counts
    open(unit=10, file='namelist_repeat.dat', status='replace', form='formatted')
    write(10, '(A)') ' &REPEAT'
    write(10, '(A)') '  arr = 10*3.14'
    write(10, '(A)') '  iarr = 5*42'
    write(10, '(A)') ' /'
    close(10)

    ! Read namelist with repeat counts
    open(unit=10, file='namelist_repeat.dat', status='old', form='formatted')
    read(10, nml=repeat)
    close(10)

    ! Verify all arr elements are 3.14
    do i = 1, 10
        if (abs(arr(i) - 3.14) > 1.0e-5) then
            print *, "Error: arr(", i, ") =", arr(i), "expected 3.14"
            error stop "Repeat count test failed for arr"
        end if
    end do

    ! Verify all iarr elements are 42
    do i = 1, 5
        if (iarr(i) /= 42) then
            print *, "Error: iarr(", i, ") =", iarr(i), "expected 42"
            error stop "Repeat count test failed for iarr"
        end if
    end do

    print *, "Repeat count namelist test passed!"

end program test_repeat
