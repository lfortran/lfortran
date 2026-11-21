program allocatable_component_vector_subscript_shape_01
    implicit none

    type :: t
        integer, allocatable :: d(:)
    end type t

    type(t) :: r(2)
    integer, allocatable :: m(:,:)
    integer :: a(4) = [10, 20, 30, 40]
    integer :: mat(3, 3)

    allocate(r(1)%d(1))
    r(1)%d = a([2, 4])

    if (size(r(1)%d) /= 2) error stop 1
    if (any(r(1)%d /= [20, 40])) error stop 2

    mat = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
    allocate(m(1, 2))
    m = mat(1:2, [1, 3])

    if (size(m, 1) /= 2 .or. size(m, 2) /= 2) error stop 3
    if (any(m /= reshape([1, 2, 7, 8], [2, 2]))) error stop 4
end program allocatable_component_vector_subscript_shape_01
