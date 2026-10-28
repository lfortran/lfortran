program allocate_source_stat_01
    ! SOURCE= or MOLD= supplies the bounds when other alloc-opts follow
    implicit none
    integer, allocatable :: a(:), b(:), c(:), m2(:,:), s2(:,:), e(:)
    integer, allocatable :: k
    character(len=:), allocatable :: str
    character(len=32) :: msg
    integer :: ierr

    allocate(a(3))
    a = [1, 2, 3]

    ierr = -1
    allocate(b, source=a, stat=ierr)
    if (ierr /= 0) error stop 1
    if (size(b) /= 3) error stop 2
    if (any(b /= [1, 2, 3])) error stop 3

    ierr = -1
    allocate(c, mold=a, stat=ierr)
    if (ierr /= 0) error stop 4
    if (size(c) /= 3) error stop 5

    allocate(m2(2:3, -1:1))
    m2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    ierr = -1
    msg = "unchanged"
    allocate(s2, source=m2, stat=ierr, errmsg=msg)
    if (ierr /= 0) error stop 6
    if (msg /= "unchanged") error stop 7
    if (lbound(s2, 1) /= 2 .or. ubound(s2, 1) /= 3) error stop 8
    if (lbound(s2, 2) /= -1 .or. ubound(s2, 2) /= 1) error stop 9
    if (any(s2 /= m2)) error stop 10

    ierr = -1
    allocate(e, mold=a, errmsg=msg, stat=ierr)
    if (ierr /= 0) error stop 11
    if (size(e) /= 3) error stop 12

    ierr = -1
    allocate(k, source=42, stat=ierr)
    if (ierr /= 0) error stop 13
    if (k /= 42) error stop 14

    ierr = -1
    allocate(str, source="hello", stat=ierr)
    if (ierr /= 0) error stop 15
    if (len(str) /= 5) error stop 16
    if (str /= "hello") error stop 17

    print *, b, size(c), s2, size(e), k, str
end program
