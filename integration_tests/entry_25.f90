program entry_25
implicit none
integer :: n
n = 0
call entry_25_s(n)
print *, n
if (n /= 12) error stop
n = 0
call entry_25_e(n)
print *, n
if (n /= 2) error stop
end program

subroutine entry_25_s(n)
integer :: n
n = 10*n + 1
entry entry_25_e(n)
n = 10*n + 2
end subroutine
