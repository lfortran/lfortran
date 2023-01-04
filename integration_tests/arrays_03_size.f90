program arrays_03_size
    implicit none
    integer :: i, a(3), b(4)
    integer :: size_a
    integer :: size_b
    integer :: size_fjac
    real :: x(3), fvec(15), fjac(size(fvec), size(x))
    size_a = size(a, kind=4)
    size_b = size(b, dim=1, kind=4)

    if (size_a /= 3) error stop
    if (size_b /= 4) error stop

    ! issue - 1133
    size_fjac = size(fjac, 1)
    if (size_fjac /= 15) error stop

    i = 1
    size_fjac = size(fjac, i)
    if (size_fjac /= 15) error stop

    size_fjac = size(fjac, 2)
    if (size_fjac /= 3) error stop
end
