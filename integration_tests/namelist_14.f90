program test_char_arrays
    implicit none
    character(len=20) :: names(5)
    character(len=10) :: codes(3)
    integer :: i

    namelist /strings/ names, codes

    ! Initialize
    names(1) = 'Alice'
    names(2) = 'Bob'
    names(3) = 'Charlie'
    names(4) = 'Diana'
    names(5) = 'Eve'

    codes(1) = 'ABC'
    codes(2) = 'DEF'
    codes(3) = 'GHI'

    ! Write
    open(10, file='strings.dat', status='replace', form='formatted')
    write(10, nml=strings)
    close(10)

    ! Reset
    names = ''
    codes = ''

    ! Read
    open(10, file='strings.dat', status='old', form='formatted')
    read(10, nml=strings)
    close(10)

    ! Verify
    if (trim(names(1)) /= 'Alice') then
        print *, "Error: names(1) =", trim(names(1)), "expected Alice"
        error stop "Test failed for names(1)"
    end if

    if (trim(names(3)) /= 'Charlie') then
        print *, "Error: names(3) =", trim(names(3)), "expected Charlie"
        error stop "Test failed for names(3)"
    end if

    if (trim(codes(2)) /= 'DEF') then
        print *, "Error: codes(2) =", trim(codes(2)), "expected DEF"
        error stop "Test failed for codes(2)"
    end if

    ! Verify all names
    if (trim(names(2)) /= 'Bob') then
        print *, "Error: names(2) =", trim(names(2)), "expected Bob"
        error stop "Test failed for names(2)"
    end if

    if (trim(names(4)) /= 'Diana') then
        print *, "Error: names(4) =", trim(names(4)), "expected Diana"
        error stop "Test failed for names(4)"
    end if

    if (trim(names(5)) /= 'Eve') then
        print *, "Error: names(5) =", trim(names(5)), "expected Eve"
        error stop "Test failed for names(5)"
    end if

    ! Verify all codes
    if (trim(codes(1)) /= 'ABC') then
        print *, "Error: codes(1) =", trim(codes(1)), "expected ABC"
        error stop "Test failed for codes(1)"
    end if

    if (trim(codes(3)) /= 'GHI') then
        print *, "Error: codes(3) =", trim(codes(3)), "expected GHI"
        error stop "Test failed for codes(3)"
    end if

    print *, "Character array namelist test passed!"

end program test_char_arrays
