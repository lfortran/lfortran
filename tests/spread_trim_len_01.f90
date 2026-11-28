program spread_trim_len_01
    implicit none
    character(8) :: root = '/r/'
    if (len(spread(trim(root), 1, 2)) /= 3) error stop 1
end program spread_trim_len_01
