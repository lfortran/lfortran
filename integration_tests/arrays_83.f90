program arrays_83

    integer :: anew2(17)
    integer, allocatable :: q_new(:)
    allocate(q_new(2))

    q_new = [100, 1220]
    anew2 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, q_new]

    print *, anew2
    if( any(anew2(1:15) /= [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]) ) error stop
    if( anew2(16) /= 100 ) error stop
    if( anew2(17) /= 1220 ) error stop

end program arrays_83
