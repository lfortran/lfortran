program pack_char_param_fold_01
    ! pack of constant character arrays is folded at compile time, so it can
    ! initialize character parameters (#15051)
    implicit none
    character(2), parameter :: s(3) = ["ab", "cd", "ef"]
    logical, parameter :: m(3) = [.true., .false., .true.]
    character(2), parameter :: t(*) = pack(s, [.true., .false., .true.])
    character(2), parameter :: t2(2) = pack(s, m)
    character(2), parameter :: tv(4) = pack(s, m, ["w1", "x2", "y3", "z4"])
    character(2), parameter :: ta(*) = pack(s, .true.)
    character(2), parameter :: tn(*) = pack(s, .false.)
    character(2), parameter :: tf(*) = pack(s, [.false., .false., .false.])
    character(1), parameter :: c1(*) = pack(["x", "y", "z", "w"], &
        [.false., .true., .true., .false.])
    character(3), parameter :: c3(*) = pack(["abc", "def", "ghi"], &
        [.true., .false., .true.], ["123", "456", "789"])
    character(3), parameter :: c3s(3) = pack(["abc", "def", "ghi"], .false., &
        ["123", "456", "789"])
    character(2), allocatable :: r(:)
    character(2) :: r2(2)
    integer :: i

    if (size(t) /= 2) error stop 1
    if (len(t) /= 2) error stop 2
    if (t(1) /= "ab" .or. t(2) /= "ef") error stop 3

    if (t2(1) /= "ab" .or. t2(2) /= "ef") error stop 4

    if (tv(1) /= "ab" .or. tv(2) /= "ef") error stop 5
    if (tv(3) /= "y3" .or. tv(4) /= "z4") error stop 6

    if (size(ta) /= 3) error stop 7
    do i = 1, 3
        if (ta(i) /= s(i)) error stop 8
    end do
    if (size(tn) /= 0) error stop 9
    if (size(tf) /= 0) error stop 10

    if (size(c1) /= 2 .or. len(c1) /= 1) error stop 11
    if (c1(1) /= "y" .or. c1(2) /= "z") error stop 12

    if (size(c3) /= 3 .or. len(c3) /= 3) error stop 13
    if (c3(1) /= "abc" .or. c3(2) /= "ghi" .or. c3(3) /= "789") error stop 14

    if (c3s(1) /= "123" .or. c3s(2) /= "456" .or. c3s(3) /= "789") error stop 15

    ! pack of character constants used at run time
    print *, pack(s, m)
    print *, pack(["ab", "cd", "ef"], [.true., .false., .true.])
    r2 = pack(s, m)
    if (r2(1) /= "ab" .or. r2(2) /= "ef") error stop 16
    allocate(r(2))
    r = pack(s, m)
    if (size(r) /= 2) error stop 17
    if (r(1) /= "ab" .or. r(2) /= "ef") error stop 18
    if (size(pack(s, m)) /= 2) error stop 19
    if (any(pack(["ab", "cd", "ef"], [.false., .true., .true.]) /= ["cd", "ef"])) &
        error stop 20
end program
