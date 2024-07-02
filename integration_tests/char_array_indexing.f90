program character_array_indexing
    character(len=2), dimension(2) :: b
    character(len=2), dimension(2) :: a
    b(1) = "xi"
    b(2) = "xp"
    if (len(b(2:1:-2)) /= 2) error stop
    ! if (size(b(2:1:-2)) /= 1) error stop

    ! a(1:2) = b(1:2)
    ! if (a(1) /= "xi") error stop
    ! if (a(2) /= "xp") error stop

    ! a(1:2) = b(2:1:-1)
    ! if (a(1) /= "xp") error stop
    ! if (a(2) /= "xi") error stop
end program character_array_indexing
