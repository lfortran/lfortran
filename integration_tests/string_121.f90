program string_121
! Constant substrings of non-parameter strings must use the current value,
! not the initializer
implicit none
type :: t
    character(len=4) :: s = "abcd"
end type
type(t) :: x
character(len=4) :: local = "abcd"
character(len=4), parameter :: c = "efgh"
character(len=2), parameter :: c2 = c(2:3)
integer :: i

x%s = "_hid"
local = "mnop"
i = 1

print *, x%s(1:1), x%s(i:i), x%s(2:3)
if (x%s(1:1) /= "_") error stop
if (x%s(i:i) /= "_") error stop
if (x%s(2:3) /= "hi") error stop

print *, local(1:1), local(3:4)
if (local(1:1) /= "m") error stop
if (local(3:4) /= "op") error stop
if (len(local(3:1)) /= 0) error stop
if (local(3:1) /= "") error stop

print *, c(4:4), c2
if (c(4:4) /= "h") error stop
if (c2 /= "fg") error stop
end program
