! This test checks 2 things:
! For reads into integer variables, ensure that the end delimeter is correct
! Multiple variables read from single string

program read_64
    implicit none

    ! Variables for valid integer read checks
    integer :: val, istat
    character(len=6) :: str

    ! Variables for multi-variable read checks
    integer, parameter :: n = 3
    integer, dimension(:), allocatable :: a, b
    character(len=6), dimension(n) :: lines
    integer :: i

    integer :: expected_a(n) = [5, 3, 9]
    integer :: expected_b(n) = [42, 7, 15]

    istat = 0
    val = 5
    str = '577;'
    
    ! Check invalid list-directed reads
    ! It should not update val, since passed string is not valid for integer read
    read(str, *, iostat=istat) val
    print *, "--- Test 1 ---"
    print *, str, val, istat
    if (val /= 5) error stop 
    if (istat == 0) error stop

    str = '576,'
    
    ! Check invalid list-directed reads
    ! It should update val, since passed string end delimeter is valid for integer read
    read(str, *, iostat=istat) val
    print *, str, val, istat
    if (val /= 576) error stop 
    if (istat /= 0) error stop

    print *, "--- Test 2: Multi-variable read ---"
    allocate(a(n), b(n))

    ! Check read into multiple variables from a single string
    lines(1) = '5   42'
    lines(2) = '3   7'
    lines(3) = '9   15'

    do i = 1, n
        read(lines(i), *) a(i), b(i)
        print *, i, ": '", lines(i), " a =", a(i), "b =", b(i)
    end do

    if (any(a /= expected_a) .or. any(b /= expected_b)) error stop

end program read_64