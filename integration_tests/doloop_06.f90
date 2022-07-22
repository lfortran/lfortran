program doloop_06
    implicit none
    integer :: i, j, k, cnt
    cnt = 0
    do i = 1, 10
        do j = 11, 20
            do k = 101, 110
                cnt = cnt + 1
            end do
        end do
    end do

    print *, cnt
    if (cnt /= 1000) error stop
end
