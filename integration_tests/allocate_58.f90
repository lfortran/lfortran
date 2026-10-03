program allocate_58
    character(len=5) :: a
    character(len=:), allocatable :: b

    allocate(b, mold=a)

    if (len(b) /= 5) error stop
end program
