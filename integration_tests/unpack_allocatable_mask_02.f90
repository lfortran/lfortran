program unpack_allocatable_mask_02
    ! An unallocated allocatable array is allocated with the shape of MASK
    ! and, for a deferred-length character array, the length of VECTOR when
    ! the result of unpack is assigned to it, also for a zero-size MASK
    ! (#17270).
    implicit none
    real, allocatable :: vals(:), out(:, :)
    logical, allocatable :: slots(:, :), m(:)
    character(len=:), allocatable :: cv(:), cout(:)

    allocate(vals(3), slots(2, 2))
    vals = [1.5, 2.5, 4.0]
    slots = reshape([.true., .false., .true., .true.], [2, 2])

    out = unpack(vals, slots, 0.0)
    if (any(shape(out) /= [2, 2])) error stop 1
    if (any(lbound(out) /= [1, 1])) error stop 2
    if (any(out(:, 1) /= [1.5, 0.0])) error stop 3
    if (any(out(:, 2) /= [2.5, 4.0])) error stop 4

    deallocate(slots)
    allocate(slots(3, 1))
    slots(:, 1) = [.false., .true., .true.]
    out = unpack(vals, slots, -1.0)
    if (any(shape(out) /= [3, 1])) error stop 5
    if (any(out(:, 1) /= [-1.0, 1.5, 2.5])) error stop 6

    deallocate(vals, slots)
    allocate(vals(0), slots(0, 3))
    out = unpack(vals, slots, 0.0)
    if (any(shape(out) /= [0, 3])) error stop 7

    allocate(character(len=3) :: cv(2))
    allocate(m(3))
    cv = ["abc", "def"]
    m = [.true., .false., .true.]
    cout = unpack(cv, m, "zzz")
    if (len(cout) /= 3) error stop 8
    if (size(cout) /= 3) error stop 9
    if (any(cout /= ["abc", "zzz", "def"])) error stop 10

    print *, "OK"
end program
