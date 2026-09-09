! A logical scalar of a width the device has is a kernel argument. A
! BLOCK nested in IF used to be stolen when the launch then rejected
! that logical. The loop must offload and the BLOCK must still compute.
program gpu_metal_300
implicit none
logical :: flag
integer :: a(4), b(4), i

flag = .true.
a = 0
b = 0

do concurrent (i = 1:4)
    if (flag) then
        block
            integer :: t
            t = i * 2
            a(i) = t
        end block
    else
        a(i) = -1
    end if
end do

do i = 1, 4
    if (a(i) /= i * 2) error stop "if-block"
end do

flag = .false.
do concurrent (i = 1:4)
    if (flag) then
        b(i) = -1
    else
        block
            integer :: t
            t = i + 10
            b(i) = t
        end block
    end if
end do

do i = 1, 4
    if (b(i) /= i + 10) error stop "else-block"
end do

print *, "PASS"
end program
