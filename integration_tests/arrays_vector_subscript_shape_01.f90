program arrays_vector_subscript_shape_01
    implicit none

    type :: t
        integer, allocatable :: d(:)
    end type t

    type(t) :: r(2)
    integer, allocatable :: m(:,:)
    integer :: a(4) = [10, 20, 30, 40]
    integer :: b(4), c(2), mat(3, 3)
    integer :: i, j

    i = 2
    j = 4

    allocate(r(1)%d(2))
    r(1)%d = a([i, j])
    if (any(r(1)%d /= [20, 40])) error stop 1

    b = 0
    b(2:3) = a([2, 4])
    if (any(b /= [0, 20, 40, 0])) error stop 2

    call assign_vec(c, a, [4, 1])
    if (any(c /= [40, 10])) error stop 3

    mat = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
    allocate(m(2, 2))
    m = mat(1:2, [1, 3])
    if (any(m /= reshape([1, 2, 7, 8], [2, 2]))) error stop 4

contains

    subroutine assign_vec(dst, src, ix)
        integer, intent(out) :: dst(:)
        integer, intent(in) :: src(:), ix(:)
        dst = src(ix)
    end subroutine assign_vec

end program arrays_vector_subscript_shape_01
