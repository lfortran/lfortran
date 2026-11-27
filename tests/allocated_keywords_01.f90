program allocated_keywords_01
    implicit none
    integer, allocatable :: s
    integer, allocatable :: a(:)
    print *, allocated(scalar=s), allocated(array=a)
    allocate(s, a(2))
    print *, allocated(scalar=s), allocated(array=a)
end program allocated_keywords_01
