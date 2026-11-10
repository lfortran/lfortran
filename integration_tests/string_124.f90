module string_124_mod
    ! module character array initialized with a scalar constant (#4514)
    implicit none
    character(100) :: arr2(10) = "abcd"
end module

program string_124
    use string_124_mod
    implicit none
    integer :: i

    print *, trim(arr2(1)), " ", trim(arr2(10))
    if (size(arr2) /= 10) error stop
    if (len(arr2) /= 100) error stop
    do i = 1, 10
        if (arr2(i) /= "abcd") error stop
        if (len_trim(arr2(i)) /= 4) error stop
    end do
    if (arr2(5)(5:100) /= "") error stop
    arr2(3) = "xyz"
    if (arr2(3) /= "xyz") error stop
    if (arr2(4) /= "abcd") error stop
end program
