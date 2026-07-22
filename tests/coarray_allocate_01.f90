program coarray_allocate_01
    implicit none
    integer, allocatable :: a[:]
    integer :: stat
    character(len=100) :: errmsg

    allocate(a[*], stat=stat, errmsg=errmsg)
    a = this_image()
    deallocate(a, stat=stat, errmsg=errmsg)
end program
