program entry_18
implicit none
integer :: j
real :: s

interface
    function rf1(x)
        real, intent(in) :: x
        real :: rf1
    end function
end interface

call sub1(1, j)
if (j /= 2) error stop
call ent1(1.0, s, rf1)
if (abs(s - 2.0) > 1.0e-6) error stop
print *, j, s
end program

subroutine sub1(ia, ib)
ib = ia + 1
return
entry ent1(ra, rb, rfd)
rb = rfd(ra)
return
end subroutine

function rf1(x)
implicit none
real, intent(in) :: x
real :: rf1
rf1 = x * 2.0
end function
