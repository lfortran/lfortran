program coarray_allocate_01
    implicit none
    integer, allocatable, save :: a[:]
    integer, allocatable, save :: b(:)[:]    
    integer :: stat
    character(len=100) :: errmsg
    allocate(a[*], b(10)[*], stat=stat, errmsg=errmsg)
    a = this_image()
    deallocate(a, b, stat=stat, errmsg=errmsg)
end program
