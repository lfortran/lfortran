program allocate_errmsg_01
    implicit none
    integer, allocatable :: a(:)
    integer :: s
    allocate(a(2), stat=s, errmsg='x')
end program allocate_errmsg_01
