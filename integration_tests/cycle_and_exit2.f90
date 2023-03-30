program test_loops
integer :: i, j, k
l1 : do i = 1, 5
    print *, "l1 start"
    l2 : do j = 1, 4
        print *, "l2 start"
        l3 : do k = 1, 2
            if (k > 0) then
                cycle l1
            end if
            print *, "l1", i, "l2", j, "l3", k
            if (k > 0) then
                print *, "error l2"
            end if
        end do l3
    end do l2
end do l1
if (j == 1 .and. k == 1) then
    print *, "pass"
else
    print *, "error", i, j, k
end if
end program