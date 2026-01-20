program namelist_test_03
    implicit none

    ! Define variables
    integer :: i1, i2
    real :: r1, r2
    logical :: l1, l2
    character(len=10) :: c1, c2
    integer :: arr1(3)
    real :: arr2(2)

    ! Define namelist
    namelist /testdata/ i1, i2, r1, r2, l1, l2, c1, c2, arr1, arr2

    ! Initialize variables
    i1 = 42
    i2 = -17
    r1 = 3.14159
    r2 = -2.71828
    l1 = .true.
    l2 = .false.
    c1 = 'hello'
    c2 = 'world'
    arr1(1) = 1
    arr1(2) = 2
    arr1(3) = 3
    arr2(1) = 1.5
    arr2(2) = 2.5

    ! Write namelist to file
    open(unit=10, file='namelist_test.dat', status='replace', form='formatted')
    write(10, nml=testdata)
    close(10)

    ! Reset variables to different values
    i1 = 0
    i2 = 0
    r1 = 0.0
    r2 = 0.0
    l1 = .false.
    l2 = .true.
    c1 = 'xxxxx'
    c2 = 'yyyyy'
    arr1(1) = 0
    arr1(2) = 0
    arr1(3) = 0
    arr2(1) = 0.0
    arr2(2) = 0.0

    ! Read namelist from file
    open(unit=10, file='namelist_test.dat', status='old', form='formatted')
    read(10, nml=testdata)
    close(10)

    ! Verify values
    if (i1 /= 42) error stop "Read i1 mismatch"
    if (i2 /= -17) error stop "Read i2 mismatch"
    if (abs(r1 - 3.14159) > 1.0e-5) error stop "Read r1 mismatch"
    if (abs(r2 - (-2.71828)) > 1.0e-5) error stop "Read r2 mismatch"
    if (.not. l1) error stop "Read l1 mismatch"
    if (l2) error stop "Read l2 mismatch"
    if (trim(c1) /= 'hello') error stop "Read c1 mismatch"
    if (trim(c2) /= 'world') error stop "Read c2 mismatch"
    if (arr1(1) /= 1) error stop "Read arr1(1) mismatch"
    if (arr1(2) /= 2) error stop "Read arr1(2) mismatch"
    if (arr1(3) /= 3) error stop "Read arr1(3) mismatch"
    if (abs(arr2(1) - 1.5) > 1.0e-5) error stop "Read arr2(1) mismatch"
    if (abs(arr2(2) - 2.5) > 1.0e-5) error stop "Read arr2(2) mismatch"

    print *, ""
    print *, "All namelist tests passed!"

end program namelist_test_03
