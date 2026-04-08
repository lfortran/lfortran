program arrays_reshape_41
    implicit none
    real(8), allocatable :: x(:), v(:)

    allocate(x(6))
    x = [1.d0, 2.d0, 3.d0, 4.d0, 5.d0, 6.d0]
    v = f(reshape(x, [size(x)]))

    if (.not. allocated(v)) error stop 1
    if (size(v) /= size(x)) error stop 2
    if (any(v /= x)) error stop 3

contains

    function f(r) result(v)
        real(8), intent(in) :: r(:)
        real(8) :: v(size(r))

        v = r
    end function f

end program arrays_reshape_41
