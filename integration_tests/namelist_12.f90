program test_iostat
    implicit none

    integer :: arr(10)
    real :: val
    integer :: iostat_val
    integer :: i

    namelist /test/ arr, val

    ! Test 1: Successful read (iostat = 0)
    arr = -999
    val = -999.0

    open(unit=10, file='namelist_iostat_success.dat', status='replace', form='formatted')
    write(10, '(A)') ' &TEST'
    write(10, '(A)') '  arr = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10'
    write(10, '(A)') '  val = 42.5'
    write(10, '(A)') ' /'
    close(10)

    open(unit=10, file='namelist_iostat_success.dat', status='old', form='formatted')
    read(10, nml=test, iostat=iostat_val)
    close(10)

    if (iostat_val /= 0) then
        print *, "Error: Expected iostat=0 for successful read, got", iostat_val
        error stop "Test 1 failed"
    end if

    if (arr(5) /= 5 .or. abs(val - 42.5) > 1.0e-5) then
        print *, "Error: Data read incorrectly"
        error stop "Test 1 failed - data mismatch"
    end if

    print *, "Test 1 passed: Successful read with iostat=0"

    ! Test 2: Array index out of bounds (iostat = 5015)
    arr = -999

    open(unit=10, file='namelist_iostat_bounds.dat', status='replace', form='formatted')
    write(10, '(A)') ' &TEST'
    write(10, '(A)') '  arr(20) = 99'  ! Index 20 is out of bounds (array size is 10)
    write(10, '(A)') ' /'
    close(10)

    open(unit=10, file='namelist_iostat_bounds.dat', status='old', form='formatted')
    read(10, nml=test, iostat=iostat_val)
    close(10)

    if (iostat_val /= 5015) then
        print *, "Error: Expected iostat=5015 for bounds error, got", iostat_val
        error stop "Test 2 failed"
    end if

    print *, "Test 2 passed: Bounds error detected with iostat=5015"

    ! Test 3: Repeat count overflow (iostat = 5015)
    arr = -999

    open(unit=10, file='namelist_iostat_repeat.dat', status='replace', form='formatted')
    write(10, '(A)') ' &TEST'
    write(10, '(A)') '  arr = 15*1'  ! Repeat count 15 exceeds array size 10
    write(10, '(A)') ' /'
    close(10)

    open(unit=10, file='namelist_iostat_repeat.dat', status='old', form='formatted')
    read(10, nml=test, iostat=iostat_val)
    close(10)

    if (iostat_val /= 5015) then
        print *, "Error: Expected iostat=5015 for repeat overflow, got", iostat_val
        error stop "Test 3 failed"
    end if

    print *, "Test 3 passed: Repeat count overflow detected with iostat=5015"

    ! Test 4: Unknown variable (iostat = 5012)
    arr = -999

    open(unit=10, file='namelist_iostat_unknown.dat', status='replace', form='formatted')
    write(10, '(A)') ' &TEST'
    write(10, '(A)') '  unknown_var = 123'
    write(10, '(A)') ' /'
    close(10)

    open(unit=10, file='namelist_iostat_unknown.dat', status='old', form='formatted')
    read(10, nml=test, iostat=iostat_val)
    close(10)

    if (iostat_val /= 5012) then
        print *, "Error: Expected iostat=5012 for unknown variable, got", iostat_val
        error stop "Test 4 failed"
    end if

    print *, "Test 4 passed: Unknown variable detected with iostat=5012"

    ! Test 5: Group not found (iostat = 5010)
    arr = -999

    open(unit=10, file='namelist_iostat_notfound.dat', status='replace', form='formatted')
    write(10, '(A)') ' &WRONG_GROUP'
    write(10, '(A)') '  arr = 1, 2, 3'
    write(10, '(A)') ' /'
    close(10)

    open(unit=10, file='namelist_iostat_notfound.dat', status='old', form='formatted')
    read(10, nml=test, iostat=iostat_val)
    close(10)

    if (iostat_val /= 5010) then
        print *, "Error: Expected iostat=5010 for group not found, got", iostat_val
        error stop "Test 5 failed"
    end if

    print *, "Test 5 passed: Group not found detected with iostat=5010"

    print *, "All IOSTAT error handling tests passed!"

end program test_iostat
