program doloop_07
    implicit none
    integer :: i, cnt
    cnt = 0
    do i = 1, 10
        if (i <= 5) then
            if (i == 1) then
                cnt = cnt + 1
                if ( cnt == 1 ) exit
            end if
        end if
        cnt = cnt + 1
    end do

    print *, cnt
    if (cnt /= 1) error stop
end
