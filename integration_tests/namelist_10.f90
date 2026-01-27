program test_indexing
    implicit none

    ! Test array indexing syntax: arr(i,j) = value
    integer :: grid(3,3)
    real :: vec(5)
    integer :: i, j

    namelist /indexed/ grid, vec

    ! Initialize to zeros
    grid = 0
    vec = 0.0

    ! Create input file with array indexing
    open(unit=10, file='namelist_indexed.dat', status='replace', form='formatted')
    write(10, '(A)') ' &INDEXED'
    write(10, '(A)') '  grid(2,2) = 99'
    write(10, '(A)') '  grid(1,3) = 42'
    write(10, '(A)') '  grid(3,1) = 17'
    write(10, '(A)') '  vec(2) = 2.5'
    write(10, '(A)') '  vec(4) = 4.5'
    write(10, '(A)') ' /'
    close(10)

    ! Read namelist with array indexing
    open(unit=10, file='namelist_indexed.dat', status='old', form='formatted')
    read(10, nml=indexed)
    close(10)

    ! Verify grid(2,2) = 99
    if (grid(2,2) /= 99) then
        print *, "Error: grid(2,2) =", grid(2,2), "expected 99"
        error stop "Array indexing test failed for grid(2,2)"
    end if

    ! Verify grid(1,3) = 42
    if (grid(1,3) /= 42) then
        print *, "Error: grid(1,3) =", grid(1,3), "expected 42"
        error stop "Array indexing test failed for grid(1,3)"
    end if

    ! Verify grid(3,1) = 17
    if (grid(3,1) /= 17) then
        print *, "Error: grid(3,1) =", grid(3,1), "expected 17"
        error stop "Array indexing test failed for grid(3,1)"
    end if

    ! Verify vec(2) = 2.5
    if (abs(vec(2) - 2.5) > 1.0e-5) then
        print *, "Error: vec(2) =", vec(2), "expected 2.5"
        error stop "Array indexing test failed for vec(2)"
    end if

    ! Verify vec(4) = 4.5
    if (abs(vec(4) - 4.5) > 1.0e-5) then
        print *, "Error: vec(4) =", vec(4), "expected 4.5"
        error stop "Array indexing test failed for vec(4)"
    end if

    ! Verify other elements remain zero
    do j = 1, 3
        do i = 1, 3
            if ((i /= 2 .or. j /= 2) .and. (i /= 1 .or. j /= 3) .and. (i /= 3 .or. j /= 1)) then
                if (grid(i,j) /= 0) then
                    print *, "Error: grid(", i, ",", j, ") =", grid(i,j), "expected 0"
                    error stop "Unmodified grid element changed"
                end if
            end if
        end do
    end do

    if (abs(vec(1)) > 1.0e-5 .or. abs(vec(3)) > 1.0e-5 .or. abs(vec(5)) > 1.0e-5) then
        error stop "Unmodified vec element changed"
    end if

    print *, "Array indexing namelist test passed!"

end program test_indexing
