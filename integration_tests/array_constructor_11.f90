program array_constructor_11
    ! An array constructor whose element is a section with an integer(8)
    ! bound, or (as array_constructor_12, with --descriptor-index-64) with
    ! integer(4) bounds: the size of the constructor is computed from the
    ! section's extent in the kind of its triplet. An implied-do with an
    ! integer(8) variable next to a run-time-sized element: its trip count
    ! is computed in integer(8) and only then converted to the index kind.
    implicit none
    integer, parameter :: arr(2, 2) = reshape([10, 20, 30, 40], [2, 2])
    integer, parameter :: sub(2) = [arr(1_8:, 1)]
    integer, parameter :: sub2(2) = [arr(1_8:2_8, 2)]
    integer :: x(5), y(2), z(4)
    integer(8) :: k, j, n8, big
    integer :: i
    real, allocatable :: t(:)
    real :: a4(4), a5(5), a6(6)
    x = [1, 2, 3, 4, 5]
    k = 2
    i = 2
    y = [x(k:3)]
    if (y(1) /= 2 .or. y(2) /= 3) error stop 1
    y = [x(i:3)]
    if (y(1) /= 2 .or. y(2) /= 3) error stop 2
    z = [x(k:5:2), x(:k)]
    if (any(z /= [2, 4, 1, 2])) error stop 3
    if (sub(1) /= 10 .or. sub(2) /= 20) error stop 4
    if (sub2(1) /= 30 .or. sub2(2) /= 40) error stop 5
    y = [arr(:, 1)]
    if (y(1) /= 10 .or. y(2) /= 20) error stop 6
    allocate(t(2))
    t = [3.0, 4.0]
    n8 = 2
    a5 = [1.0, (t, j = 1_8, n8)]
    if (any(abs(a5 - [1.0, 3.0, 4.0, 3.0, 4.0]) > 1e-6)) error stop 7
    big = 2147483646_8
    a6 = [t, (real(j - big), j = big, big + 3)]
    if (any(abs(a6 - [3.0, 4.0, 0.0, 1.0, 2.0, 3.0]) > 1e-6)) error stop 8
    a4 = [t, (real(j - big), j = big, big + 3, 2_8)]
    if (any(abs(a4 - [3.0, 4.0, 0.0, 2.0]) > 1e-6)) error stop 9
    print *, y, z
end program
