program arrays_140
    use iso_fortran_env, only: int8, int16, int64
    implicit none

    integer :: a(3, 3), b(2, 2), c(3, 3)
    integer, allocatable :: alloc_b(:, :)
    integer :: idx(3)
    integer(int64) :: lo8, hi8, st8
    integer(int8) :: lo1, hi1, st1
    integer(int16) :: lo2, hi2, st2
    integer :: i, j

    do j = 1, 3
        do i = 1, 3
            a(i, j) = i + 10 * j
        end do
    end do
    idx = [3, 1, 2]

    ! integer(8) literal lower bound, upper bound, and step with vector subscript
    b = a(1_int64:3:2, [3, 1])
    if (any(b /= reshape([31, 33, 11, 13], [2, 2]))) error stop 1

    b = a(1:2_int64, [2, 3])
    if (any(b /= reshape([21, 22, 31, 32], [2, 2]))) error stop 2

    b = a(1:3:2_int64, [1, 2])
    if (any(b /= reshape([11, 13, 21, 23], [2, 2]))) error stop 3

    ! integer(8) variable bounds with negative step
    lo8 = 3_int64
    hi8 = 1_int64
    st8 = -2_int64
    b = a(lo8:hi8:st8, [2, 1])
    if (any(b /= reshape([23, 21, 13, 11], [2, 2]))) error stop 4

    ! integer(1) and integer(2) triplet bounds and step with vector subscript
    lo1 = 1_int8
    hi1 = 3_int8
    st1 = 2_int8
    b = a(lo1:hi1:st1, [1, 3])
    if (any(b /= reshape([11, 13, 31, 33], [2, 2]))) error stop 5

    lo2 = 3_int16
    hi2 = 1_int16
    st2 = -2_int16
    b = a(lo2:hi2:st2, [3, 2])
    if (any(b /= reshape([33, 31, 23, 21], [2, 2]))) error stop 6

    ! LHS target with integer(8) triplet and vector subscript
    c = 0
    c(1_int64:3_int64:2_int64, [3, 1]) = reshape([101, 102, 103, 104], [2, 2])
    if (c(1, 3) /= 101 .or. c(3, 3) /= 102 .or. c(1, 1) /= 103 .or. c(3, 1) /= 104) error stop 7
    if (c(2, 1) /= 0 .or. c(2, 3) /= 0 .or. any(c(:, 2) /= 0)) error stop 8

    ! Allocatable LHS (exercises realloc path under --realloc-lhs-arrays / --std=f23)
    allocate(alloc_b(2, 2))
    alloc_b = a(1_int64:3_int64:2_int64, [3, 1])
    if (any(alloc_b /= reshape([31, 33, 11, 13], [2, 2]))) error stop 9

    ! Vector subscript that is itself a 1D array section with integer(8) triplet
    b = a(1_int64:2_int64, idx(1_int64:2_int64))
    if (any(b /= reshape([31, 32, 11, 12], [2, 2]))) error stop 10

    print *, "ok"
end program
