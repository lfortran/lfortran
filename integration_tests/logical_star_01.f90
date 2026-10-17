program logical_star_01
    implicit none
    logical*1 :: l1, l1_arr(3)
    logical*2 :: l2
    logical*4 :: l4
    logical*8 :: l8

    if (kind(l1) /= 1) error stop
    if (kind(l1_arr) /= 1) error stop
    if (kind(l2) /= 2) error stop
    if (kind(l4) /= 4) error stop
    if (kind(l8) /= 8) error stop

    if (storage_size(l1) /= 8) error stop
    if (storage_size(l2) /= 16) error stop
    if (storage_size(l4) /= 32) error stop
    if (storage_size(l8) /= 64) error stop

    l1 = .true.
    l1_arr = [.true., .false., .true.]
    l2 = .false.
    l4 = l1 .and. .not. l2
    l8 = count(l1_arr) == 2
    if (.not. l4 .or. .not. l8) error stop
    print *, "ok"
end program
