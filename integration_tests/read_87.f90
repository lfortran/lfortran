program read_87
    implicit none

    character(4) :: s1
    integer :: i1
    logical :: l1
    real :: r1
    integer :: unit_no
    character(20), parameter :: instring = "'TWO ', 2, , 2.0"

    open (newunit=unit_no, status='scratch', form='formatted')
    write (unit_no, '(a)') instring
    rewind (unit_no)

    s1 = 'nono'
    i1 = -42
    l1 = .false.
    r1 = -42.42

    read (unit_no, *) s1, i1, l1, r1
    if (s1 /= 'TWO') error stop 1
    if (i1 /= 2) error stop 2
    if (l1) error stop 3
    if (r1 /= 2.0) error stop 4

    l1 = .true.
    rewind (unit_no)
    read (unit_no, *) s1, i1, l1, r1
    if (.not. l1) error stop 5

    close (unit_no)
end program read_87
