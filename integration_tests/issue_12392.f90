program issue12392
implicit none
character(len=5) :: names(4)
names(1) = "aaaaa"
names(2) = "bbbbb"
names(3) = "ccccc"
names(4) = "ddddd"
call CSHIFT8_ffl(names, 2.0)
print *, names
if (names(1) /= "ccccc") error stop
if (names(2) /= "ddddd") error stop
if (names(3) /= "aaaaa") error stop
if (names(4) /= "bbbbb") error stop

contains

subroutine CSHIFT8_ffl(a, s)
implicit none
character(*) :: a(:)
real :: s
a = CSHIFT(a, 2)
end subroutine

end program
