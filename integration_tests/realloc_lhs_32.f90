program realloc_lhs_32
    ! An allocatable assigned an array expression of its own shape is not
    ! reallocated, so it keeps its bounds and its storage (#15956)
    implicit none
    integer, allocatable, target :: a(:), m(:,:)
    integer, pointer :: p(:), q(:,:)
    integer :: b(3), d(2,3)

    b = [1, 2, 3]
    allocate(a(0:2))
    a = b
    p => a
    a = b + 1
    if (lbound(a, 1) /= 0 .or. ubound(a, 1) /= 2) error stop "rank 1 bounds"
    if (.not. associated(p, a)) error stop "rank 1 reallocated"
    if (any(a /= [2, 3, 4])) error stop "rank 1 contents"

    a = 2 * b
    if (lbound(a, 1) /= 0) error stop "rank 1 bounds, product"
    if (any(a /= [2, 4, 6])) error stop "rank 1 contents, product"

    d = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    allocate(m(-1:0, 5:7))
    m = d
    q => m
    m = d + 1
    if (any(lbound(m) /= [-1, 5])) error stop "rank 2 bounds"
    if (.not. associated(q, m)) error stop "rank 2 reallocated"
    if (any(m /= d + 1)) error stop "rank 2 contents"

    ! Another shape is still reallocated, with lower bounds of 1
    a = [b, b] + 1
    if (lbound(a, 1) /= 1 .or. size(a) /= 6) error stop "other shape"
    if (any(a /= [2, 3, 4, 2, 3, 4])) error stop "other shape contents"
    print *, a
end program
