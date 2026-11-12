program unpack_allocatable_mask_01
    ! The result of unpack has the shape of MASK. A temporary that holds it
    ! inside an expression is allocated with that shape when the shape is
    ! known only at run time (#17270).
    implicit none
    real, allocatable :: vals(:), out(:, :)
    logical, allocatable :: slots(:, :), m1(:), m0(:)
    integer, allocatable :: ivals(:)
    integer :: ifield(5)
    character(len=3) :: cv(3)

    allocate(vals(3), slots(2, 2))
    vals = [1.5, 2.5, 4.0]
    slots = reshape([.true., .false., .true., .true.], [2, 2])

    if (sum(unpack(vals, slots, 0.0)) /= 8.0) error stop 1
    if (count(unpack(vals, slots, 0.0) > 2.0) /= 2) error stop 2
    if (maxval(unpack(vals, slots, -1.0)) /= 4.0) error stop 3
    if (minval(unpack(vals, slots, -1.0)) /= -1.0) error stop 4
    if (any(shape(unpack(vals, slots, 0.0)) /= [2, 2])) error stop 5

    allocate(out(2, 2))
    out = 2.0 * unpack(vals, slots, 0.0)
    if (any(out(:, 1) /= [3.0, 0.0])) error stop 6
    if (any(out(:, 2) /= [5.0, 8.0])) error stop 7

    if (total(vals, slots) /= 8.0) error stop 8

    allocate(m1(5), ivals(3))
    m1 = [.false., .true., .true., .false., .true.]
    ivals = [10, 20, 30]
    ifield = [-1, -2, -3, -4, -5]
    if (sum(unpack(ivals, m1, ifield)) /= 55) error stop 9
    if (any(unpack(ivals, m1, ifield) + 1 /= [0, 11, 21, -3, 31])) error stop 10
    if (count(unpack(ivals, .not. m1, 0) /= 0) /= 2) error stop 11

    cv = ["abc", "def", "ghi"]
    if (any(unpack(cv, m1, "zzz") /= ["zzz", "abc", "def", "zzz", "ghi"])) error stop 12

    allocate(m0(-1:1))
    m0 = [.true., .false., .true.]
    if (any(unpack(ivals, m0, 0) /= [10, 0, 20])) error stop 13

    deallocate(vals, slots)
    allocate(vals(0), slots(0, 3))
    if (sum(unpack(vals, slots, 1.0)) /= 0.0) error stop 14
    if (size(unpack(vals, slots, 1.0)) /= 0) error stop 15

    print *, "OK"

contains

    real function total(v, m)
        real, intent(in) :: v(:)
        logical, intent(in) :: m(:, :)
        total = sum(unpack(v, m, 0.0))
    end function

end program
