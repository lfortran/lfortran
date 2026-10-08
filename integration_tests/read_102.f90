program read_102
    ! List-directed internal reads of arrays and array sections that follow
    ! other items in the same input list (issue #13273).
    implicit none
    character(40) :: line
    character(20) :: key, key2
    character(3) :: names(3)
    character(10) :: recs(2)
    integer :: read_data(4), a(6), b(2, 3), n, m, i, ios
    integer(8) :: big(3)
    integer, allocatable :: c(:)
    character(:), allocatable :: an(:)
    real :: r(3)
    real(8) :: d(2)
    logical :: l(3)
    complex :: z(2)

    ! Scalar character followed by an array section (the issue)
    line = "arr 1, 2, 3, 4"
    read_data = 0
    read(line, *) key, read_data(1:4)
    if (key /= "arr") error stop
    if (any(read_data /= [1, 2, 3, 4])) error stop

    ! Scalar character followed by the whole array
    read_data = 0
    read(line, *) key, read_data
    if (key /= "arr") error stop
    if (any(read_data /= [1, 2, 3, 4])) error stop

    ! Array section with a stride, first item starting at element 1 and 2
    line = "key 1 2 3"
    a = 0
    read(line, *) key, a(1:6:2)
    if (key /= "key") error stop
    if (any(a /= [1, 0, 2, 0, 3, 0])) error stop
    a = 0
    read(line, *) key, a(6:2:-2)
    if (any(a /= [0, 3, 0, 2, 0, 1])) error stop

    ! Array section alone with a stride
    line = "4, 5, 6"
    a = 0
    read(line, *) a(2:6:2)
    if (any(a /= [0, 4, 0, 5, 0, 6])) error stop

    ! Multiple scalars before and after the array, blank separated
    line = "7 8 10 20 30 9"
    a = 0
    read(line, *) n, m, a(2:4), i
    if (n /= 7 .or. m /= 8 .or. i /= 9) error stop
    if (any(a /= [0, 10, 20, 30, 0, 0])) error stop

    ! Quoted character first item, comma separated values
    line = "'a b', 11, 12, 13, 14"
    read_data = 0
    read(line, *) key, read_data
    if (key /= "a b") error stop
    if (any(read_data /= [11, 12, 13, 14])) error stop

    ! Two arrays in one list
    line = "1 2 3 4 5 6"
    a = 0
    read_data = 0
    read(line, *) read_data(1:2), a(1:4)
    if (any(read_data /= [1, 2, 0, 0])) error stop
    if (any(a /= [3, 4, 5, 6, 0, 0])) error stop

    ! integer(8), real, real(8), logical and complex arrays after a scalar
    line = "x 10000000000 2 3"
    read(line, *) key, big
    if (key /= "x") error stop
    if (any(big /= [10000000000_8, 2_8, 3_8])) error stop

    line = "5 1.5 2.5 3.5"
    read(line, *) n, r
    if (n /= 5) error stop
    if (any(abs(r - [1.5, 2.5, 3.5]) > 1e-6)) error stop

    line = "y 0.25d0, -1.5 z"
    read(line, *) key, d, key2
    if (key /= "y" .or. key2 /= "z") error stop
    if (any(abs(d - [0.25d0, -1.5d0]) > 1d-12)) error stop

    line = "7 T F .true. 9"
    read(line, *) n, l, m
    if (n /= 7 .or. m /= 9) error stop
    if (.not. l(1) .or. l(2) .or. .not. l(3)) error stop

    line = "w (1.0,2.0) (3.0,-4.0)"
    read(line, *) key, z
    if (key /= "w") error stop
    if (abs(z(1) - (1.0, 2.0)) > 1e-6 .or. abs(z(2) - (3.0, -4.0)) > 1e-6) error stop

    ! Character array after a scalar
    line = "3 ab cd ef"
    names = ""
    read(line, *) n, names
    if (n /= 3) error stop
    if (names(1) /= "ab" .or. names(2) /= "cd" .or. names(3) /= "ef") error stop

    ! Character array section with a stride, allocatable character array
    line = "4 gh ij"
    names = "--"
    read(line, *) n, names(1:3:2)
    if (n /= 4) error stop
    if (names(1) /= "gh" .or. names(2) /= "--" .or. names(3) /= "ij") error stop
    allocate(character(2) :: an(3))
    line = "x 'p q' r s"
    read(line, *) key, an
    if (key /= "x") error stop
    if (an(1) /= "p" .or. an(2) /= "r" .or. an(3) /= "s") error stop

    ! Allocatable array and its section
    allocate(c(5))
    c = 0
    line = "k 1 2 3 4 5"
    read(line, *) key, c
    if (any(c /= [1, 2, 3, 4, 5])) error stop
    c = 0
    read(line, *) key, c(2:5:3)
    if (any(c /= [0, 1, 0, 0, 2])) error stop

    ! Rank 2 array and rank 2 section after a scalar
    line = "m 1 2 3 4 5 6"
    b = 0
    read(line, *) key, b
    if (any(b /= reshape([1, 2, 3, 4, 5, 6], [2, 3]))) error stop
    b = 0
    read(line, *) key, b(:, 2:3)
    if (any(b /= reshape([0, 0, 1, 2, 3, 4], [2, 3]))) error stop
    b = 0
    read(line, *) key, b(2, :)
    if (any(b /= reshape([0, 1, 0, 2, 0, 3], [2, 3]))) error stop

    ! Fewer values than array elements: end of file, remaining elements kept
    line = "9 1 2"
    read_data = 0
    read(line, *, iostat=ios) n, read_data
    if (ios >= 0) error stop
    if (n /= 9) error stop
    if (any(read_data /= [1, 2, 0, 0])) error stop

    ! Internal file with several records
    recs(1) = "1 2"
    recs(2) = "3 4"
    read_data = 0
    read(recs, *) read_data
    if (any(read_data /= [1, 2, 3, 4])) error stop

    ! Assumed-shape dummy arguments
    a = 0
    call read_into("s 4 5 6", a(1:6:2))
    if (any(a /= [4, 0, 5, 0, 6, 0])) error stop
    b = 0
    call read_into_2d("s 1 2 3 4", b(:, 1:3:2))
    if (any(b /= reshape([1, 2, 0, 0, 3, 4], [2, 3]))) error stop

    print *, "ok"

contains

    subroutine read_into(text, x)
        character(*), intent(in) :: text
        integer, intent(inout) :: x(:)
        character(10) :: k
        read(text, *) k, x
        if (k /= "s") error stop
    end subroutine

    subroutine read_into_2d(text, x)
        character(*), intent(in) :: text
        integer, intent(inout) :: x(:, :)
        character(10) :: k
        read(text, *) k, x
        if (k /= "s") error stop
    end subroutine

end program
