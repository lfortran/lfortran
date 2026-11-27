program allocated_keywords_err_01
    implicit none
    integer, allocatable :: a(:)
    print *, allocated(scalar=a)
end program allocated_keywords_err_01
