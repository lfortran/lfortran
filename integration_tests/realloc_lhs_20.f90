program realloc_lhs_20
    implicit none

    type :: t
        integer, allocatable :: arr(:)
    end type t

    type(t) :: a, b

    allocate(a%arr(0:3))
    a%arr = [2, 3, 3, 1]
    b%arr = a%arr

    if (.not. allocated(b%arr)) error stop
    if (size(b%arr) /= size(a%arr)) error stop
    if (any(b%arr /= a%arr)) error stop
end program realloc_lhs_20
