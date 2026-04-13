program allocatable_component_reshape_realloc_01
    implicit none

    type :: t
        real, allocatable :: arr(:,:)
    end type t

    type(t) :: x
    real :: input(2, 3)
    real :: gradient(6)

    input = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2, 3])
    gradient = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0]

    x%arr = reshape(gradient, shape(input))

    if (.not. allocated(x%arr)) error stop
    if (size(x%arr, 1) /= 2) error stop
    if (size(x%arr, 2) /= 3) error stop
    if (any(x%arr /= reshape([10.0, 20.0, 30.0, 40.0, 50.0, 60.0], [2, 3]))) error stop
end program allocatable_component_reshape_realloc_01
