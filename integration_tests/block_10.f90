program block_10
    implicit none
    integer :: i
    character(len=100) :: result

    result = ""

    do i = 1, 2
        block
            character(len=:), allocatable :: temp(:)
            temp = [character(len=5) :: "a", "b", "c"]
            if (i == 1) then
                result = temp(1)
            end if
        end block
    end do

    if (trim(result) /= "a") error stop "Expected: a"
    print *, "PASS"
end program
