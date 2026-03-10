program realloc_lhs_19
    implicit none
    integer, allocatable :: nz(:), result(:)

    nz = [480, 120, 1]
    allocate(result(1))

    where (nz > 1)
        result = maxval(nz)
    end where

    if (size(result) /= 1) error stop
    if (result(1) /= 480) error stop

    print *, result(1)
end program realloc_lhs_19
