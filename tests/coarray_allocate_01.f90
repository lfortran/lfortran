program coarray_allocate_01
    implicit none
    integer, allocatable :: a[:]
    integer, allocatable, save :: b(:)[:]    
    integer, allocatable :: x[:, :]
    integer :: stat
    character(len=100) :: errmsg
    allocate(a[*], b(10)[*], x[2, *], stat=stat, errmsg=errmsg)
    a = this_image()
    x = this_image()
    deallocate(a, b, x, stat=stat, errmsg=errmsg)
end program
