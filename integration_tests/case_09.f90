! Test case value range list (issue #8407)
program case_09
    implicit none
    integer :: i

    do i = 1, 5
        select case (i)
        case (2:3, 5)
            if (i /= 2 .and. i /= 3 .and. i /= 5) error stop
        case default
            if (i /= 1 .and. i /= 4) error stop
        end select
    end do
    print *, 'passed'
end program case_09
