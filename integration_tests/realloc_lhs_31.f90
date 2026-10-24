program realloc_lhs_31
    ! Reallocation on assignment happens exactly when the shape differs
    implicit none
    type :: item
        integer :: id
        integer, allocatable :: values(:)
    end type
    integer, allocatable, target :: a(:), m(:,:)
    integer, pointer :: p(:), q(:,:)
    integer :: b(3), c(5), d(2,3), e(3,2), z(0)
    type(item), allocatable :: t(:)
    type(item) :: u(2), w(3)

    ! Same shape: no reallocation, the storage stays the same
    allocate(a(3))
    a = [1, 2, 3]
    p => a
    b = [4, 5, 6]
    a = b
    if (.not. associated(p, a)) error stop "same shape reallocated"
    if (any(p /= [4, 5, 6])) error stop "same shape contents"

    ! Different extent: reallocated with the new size
    c = [10, 20, 30, 40, 50]
    a = c
    if (size(a) /= 5 .or. lbound(a, 1) /= 1) error stop "grown shape"
    if (any(a /= c)) error stop "grown contents"
    a = c(2:3)
    if (size(a) /= 2) error stop "shrunk shape"
    if (any(a /= [20, 30])) error stop "shrunk contents"

    ! Zero-size arrays, to and from
    a = z
    if (.not. allocated(a) .or. size(a) /= 0) error stop "to zero size"
    a = z
    if (size(a) /= 0) error stop "zero size to zero size"
    a = b
    if (size(a) /= 3 .or. any(a /= b)) error stop "from zero size"

    ! Rank 2: same number of elements, but other shape
    d = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    e = transpose(d)
    m = d
    q => m
    m = d + 1
    if (.not. associated(q, m)) error stop "rank 2 same shape reallocated"
    m = e
    if (size(m, 1) /= 3 .or. size(m, 2) /= 2) error stop "rank 2 other shape"
    if (any(m /= e)) error stop "rank 2 contents"
    m = transpose(m)
    if (size(m, 1) /= 2 .or. size(m, 2) /= 3) error stop "transpose shape"
    if (any(m /= d)) error stop "transpose contents"
    m = d(:, 1:0)
    if (size(m, 1) /= 2 .or. size(m, 2) /= 0) error stop "rank 2 zero size"

    ! Derived type elements, with an allocatable component
    u(1) = item(1, [1])
    u(2) = item(2, [2, 2])
    w(1) = item(3, [3])
    w(2) = item(4, [4, 4])
    w(3) = item(5, [5, 5, 5])
    t = u
    if (size(t) /= 2 .or. t(2)%id /= 2) error stop "derived first"
    t = u
    if (size(t) /= 2 .or. size(t(2)%values) /= 2) error stop "derived same shape"
    t = w
    if (size(t) /= 3 .or. t(3)%id /= 5) error stop "derived grown"
    if (any(t(3)%values /= [5, 5, 5])) error stop "derived component"
    t = w(1:1)
    if (size(t) /= 1 .or. t(1)%id /= 3) error stop "derived shrunk"
    t = w(1:0)
    if (size(t) /= 0) error stop "derived zero size"
    print *, "ok"
end program
