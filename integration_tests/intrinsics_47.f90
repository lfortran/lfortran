program intrinsics_47
    implicit none
    logical :: l = all([.true., .true.])
    logical :: a(3) = [.true., .true., .true.]
    l = all([.true., .false.])
    l = all([.false.])
    l = all([.true.])
    l = all(a)
    l = all([1 == 2, 0 == 0])
    ! TODO
    ! l = all([l])
    ! l = all([.false., a, .true.])
    if (l) error stop
end program intrinsics_47
