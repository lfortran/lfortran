! gpu decline reason: ExitOfConstruct
! An exit of a BLOCK inside an offloaded do concurrent, or inside a routine
! it calls, has no lowering in device code yet, so the loop is not
! offloaded. Each loop is reported.
module gpu_decline_20_m
implicit none
contains
pure integer function g(n) result(r)
    integer, intent(in) :: n
    r = 0
    b: block
        if (n > 2) exit b
        r = n
    end block b
end function
end module

program gpu_decline_20
use gpu_decline_20_m
implicit none
integer :: s(3), t(4), i
s = 0
do concurrent (i = 1:3)
    blk: block
        if (i == 2) exit blk
        s(i) = i
    end block blk
end do
print *, s
if (any(s /= [1, 0, 3])) error stop
t = 0
do concurrent (i = 1:4)
    t(i) = g(i)
end do
print *, t
if (any(t /= [1, 2, 0, 0])) error stop
end program
