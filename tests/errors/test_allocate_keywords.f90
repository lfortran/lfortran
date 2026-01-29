program test_allocate_keywords
    implicit none
    integer, allocatable :: x(:)
    allocate(x(10), invalid_kw=1)
end program