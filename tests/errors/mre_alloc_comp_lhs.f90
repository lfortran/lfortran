program mre_alloc_comp_lhs
    implicit none

    type :: t
        integer, allocatable :: arr(:)
    end type t

    type(t) :: a, b

    allocate(a%arr(0:3))
    a%arr = [2, 3, 3, 1]
    b%arr = a%arr
end program mre_alloc_comp_lhs
