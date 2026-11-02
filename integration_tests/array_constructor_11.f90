program array_constructor_11
    ! An array constructor whose element is a section with an integer(8)
    ! bound, or (as array_constructor_12, with --descriptor-index-64) with
    ! integer(4) bounds: the size of the constructor is computed from the
    ! section's extent in the kind of its triplet.
    implicit none
    integer, parameter :: arr(2, 2) = reshape([10, 20, 30, 40], [2, 2])
    integer, parameter :: sub(2) = [arr(1_8:, 1)]
    integer, parameter :: sub2(2) = [arr(1_8:2_8, 2)]
    integer :: x(5), y(2), z(4)
    integer(8) :: k
    integer :: i
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
    print *, y, z
end program
