program entry_24
implicit none
external entry_24_add1, entry_24_add2, entry_24_add3
integer :: n
n = 0
call entry_24_s(n, entry_24_add1, entry_24_add2, entry_24_add3)
print *, n
if (n /= 12) error stop
n = 0
call entry_24_e(entry_24_add3, entry_24_add2, n, entry_24_add1)
print *, n
if (n /= 31) error stop
end program

subroutine entry_24_s(n, f, g, h)
integer :: n
external f, g, h
call f(n)
call entry_24_pass(n, g)
return
entry entry_24_e(h, g, n, f)
call h(n)
call entry_24_pass(n, f)
end subroutine

subroutine entry_24_pass(n, p)
integer :: n
external p
call p(n)
end subroutine

subroutine entry_24_add1(n)
integer :: n
n = 10*n + 1
end subroutine

subroutine entry_24_add2(n)
integer :: n
n = 10*n + 2
end subroutine

subroutine entry_24_add3(n)
integer :: n
n = 10*n + 3
end subroutine
