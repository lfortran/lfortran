program loop_and_block
    integer :: i
    l1 : do i = 1, 10
        print *, "l1", i
        b1 : block
            if (i > 2) then
                exit l1
            end if
        end block b1
        if (i > 2) then
            print *, "error l1"
        end if
    end do l1
    if (i == 3) then
        print *, "pass"
    end if
end program