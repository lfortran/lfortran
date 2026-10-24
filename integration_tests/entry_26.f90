program entry_26
implicit none
external entry_26_add1
integer :: n
n = 0
call entry_26_s(entry_26_add1, n)
print *, n
if (n /= 1) error stop
call entry_26_e(n)
print *, n
if (n /= 11) error stop
end program

subroutine entry_26_s(f, n)
integer :: n
external f
call f(n)
return
entry entry_26_e(n)
n = n + 10
end subroutine

subroutine entry_26_add1(n)
integer :: n
n = n + 1
end subroutine
