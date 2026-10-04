program if_09
    ! Logical IF statements
    implicit none
    integer :: i, n
    n = 0
    do i = -2, 2
        select case (i)
        case (:-1)
            if (i == -2) n = n + 1 ! a comment
        case (0)
            n = n + 10
        case (1:)
            if (i == 2) &
                n = n + 100
        end select
    end do
    if (n /= 111) error stop
    i = 0
10  i = i + 1
    if (i < 3) go to 10
    if (i /= 3) error stop
end program if_09
