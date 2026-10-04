program string_122
! Assigning '' to a character array creates an ArrayConstant of
! zero-length strings
implicit none
character(len=16) :: t(3)
character(len=16) :: u(2, 2)

t = "abc"
u = "def"
call clear(t, u)

print *, len_trim(t), len_trim(u)
if (len(t) /= 16) error stop
if (size(t) /= 3) error stop
if (any(t /= "")) error stop
if (size(u) /= 4) error stop
if (any(u /= "")) error stop

contains

subroutine clear(t, u)
    character(len=16) :: t(3)
    character(len=16) :: u(2, 2)
    t = ''
    u = ''
end subroutine

end program
