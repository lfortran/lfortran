! With --realloc-lhs-arrays, a where assignment to an allocatable array keeps
! the elements where the mask is false (#14902).
program where_19
    implicit none
    integer, allocatable :: arr(:)
    integer :: brr(3)
    allocate(arr(3))
    arr = [1, 2, 3]
    brr = [1, 2, 3]
    where (brr > 1) arr = brr + 3
    if (any(arr /= [1, 5, 6])) error stop 1
    where (arr > 1)
        where (arr > 5) arr = arr + 1
        arr = arr + 3
    end where
    if (any(arr /= [1, 8, 10])) error stop 2
    print *, arr
end program
