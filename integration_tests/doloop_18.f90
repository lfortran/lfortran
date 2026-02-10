program doloop_18
implicit none
integer :: i, n

n = 3
do i = 1, n
    n = n - 1
    print *, "i,n=", i, n

    select case (i)
    case (1)
        if (n /= 2) error stop "step 1 failed: n /= 2"
    case (2)
        if (n /= 1) error stop "step 2 failed: n /= 1"
    case (3)
        if (n /= 0) error stop "step 3 failed: n /= 0"
    case default
        error stop "unexpected iteration count"
    end select
end do

! After loop completion
if (i /= 4) error stop "post-loop failed: i /= 4"
if (n /= 0) error stop "post-loop failed: n /= 0"

print *, "i,n=", i, n
end program doloop_18
