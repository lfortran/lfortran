program test_loops
integer :: i, j, k, l
l1 : do i = 1, 5
    l2 : do j = 1, 4
        if (j > 2) then
            exit l1
        end if
        print *, "l1", i, "l2", j
        if (j > 2) then
            print *, "error l2 1"
        end if
    end do l2
    if (j > 2) then
        print *, "error l2 2"
    end if
end do l1

k = 0
l3 : do while(.true.)
    l4 : do l = 1, 2
        if (k > 3) then
            exit l3
        end if
        print *, "l3", k, "l4", l
        if (k > 3) then
            print *, "error l3 1"
        end if
    end do l4
    if (k > 3) then
        print *, "error l3 2"
    end if
    k = k + 1
end do l3
if (i == 1 .and. j == 3 .and. k == 4 .and. l == 1) then
    print *, "pass"
else
    print *, "error", i, j, k, l
end if
end program