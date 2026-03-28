! Test do loop with real loop variable and real bounds
program doloop_19
    implicit none
    real :: x
    integer :: count

    ! Basic real do loop (default increment of 1.0)
    count = 0
    do x = 1.10, 3.5
        count = count + 1
    end do
    if (count /= 3) error stop

    ! Real do loop with explicit real increment
    count = 0
    do x = 0.0, 2.0, 0.5
        count = count + 1
    end do
    if (count /= 5) error stop

    ! Real do loop counting down (negative increment)
    count = 0
    do x = 3.0, 1.0, -1.0
        count = count + 1
    end do
    if (count /= 3) error stop

    print *, "ok"
end program
