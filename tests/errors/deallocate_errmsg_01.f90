program deallocate_errmsg_01
    implicit none
    integer, allocatable :: a(:)
    integer :: s
    allocate(a(2))
    deallocate(a, stat=s, errmsg='y')
end program deallocate_errmsg_01
