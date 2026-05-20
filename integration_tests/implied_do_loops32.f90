program implied_do_loops32
implicit none
character(len=2) :: a(3)
character(len=100) :: buf
integer :: i, n

a(1) = "ab"
a(2) = "cd"
a(3) = "ef"

! Variable upper bound with preceding sibling arg
n = 1
write (buf, "(2(1x,a))") "x", (trim(a(i)), i=1,n)
if (buf /= " x ab") error stop

! Variable upper bound with trailing sibling arg
n = 2
write (buf, "(3(1x,a))") (trim(a(i)), i=1,n), "y"
if (buf /= " ab cd y") error stop

! Variable upper bound with both preceding and trailing sibling args
n = 1
write (buf, "(3(1x,a))") "p", (trim(a(i)), i=1,n), "q"
if (buf /= " p ab q") error stop

! Write to stdout to verify external file output
n = 1
write (*, "(2(1x,a))") "x", (trim(a(i)), i=1,n)

print *, "PASS"
end program
