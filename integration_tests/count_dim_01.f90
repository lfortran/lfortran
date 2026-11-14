program count_dim_01
    ! count(mask, dim) of a constant mask folded at compile time
    implicit none
    integer :: i
    integer, parameter :: tbl(2,3) = reshape([4, 9, 1, 8, 2, 7], [2, 3])
    integer, parameter :: hits(3) = count(tbl > 5, dim=1)
    integer, parameter :: per_row(2) = count(tbl < 3, dim=2)
    integer, parameter :: cube(2,3,4) = reshape([(i, i = 1, 24)], [2, 3, 4])
    integer, parameter :: c1(3,4) = count(mod(cube, 3) == 0, dim=1)
    integer, parameter :: c2(2,4) = count(mod(cube, 2) == 0, dim=2)
    integer, parameter :: c3(2,3) = count(cube > 10, dim=3)
    integer(8), parameter :: k8(3) = count(tbl > 5, dim=1, kind=8)
    integer, parameter :: total = count(tbl > 5)
    integer, parameter :: vec = count([4, 9, 1] > 3, dim=1)

    if (any(hits /= [1, 1, 1])) error stop 1
    if (any(per_row /= [2, 0])) error stop 2
    if (any(c1 /= reshape([0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1], [3, 4]))) error stop 3
    if (any(c2 /= reshape([0, 3, 0, 3, 0, 3, 0, 3], [2, 4]))) error stop 4
    if (any(c3 /= reshape([2, 2, 2, 2, 3, 3], [2, 3]))) error stop 5
    if (any(k8 /= [1_8, 1_8, 1_8])) error stop 6
    if (kind(k8) /= 8) error stop 7
    if (total /= 3) error stop 8
    if (vec /= 2) error stop 9

    if (any(count(tbl > 5, dim=1) /= [1, 1, 1])) error stop 10
    if (any(count(tbl < 3, dim=2) /= [2, 0])) error stop 11
    if (any(count(cube > 10, dim=3) /= c3)) error stop 12
    if (size(count(cube > 10, dim=2)) /= 8) error stop 13

    print *, count(tbl > 5, dim=1)
    print *, count(tbl < 3, dim=2)
    print *, hits, per_row, k8
end program count_dim_01
