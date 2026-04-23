program implied_do_loops35
! Test: zero-length implied DO with trim() followed by non-zero implied DO
! with trim() in list-directed print. The zero-length loop should produce no
! output and the subsequent loop should still execute correctly.
implicit none
character(len=1) :: a(2), b(0)
character(len=100) :: buf
integer :: i

a(1) = "x"
a(2) = "y"

! Zero-length implied DO with trim() followed by non-zero implied DO with trim()
write(buf, "(*(a))") (trim(b(i)), i=1,0), (trim(a(i)), i=1,2)
if (trim(buf) /= "xy") error stop

! Single non-zero implied DO with trim()
write(buf, "(*(a))") (trim(a(i)), i=1,1)
if (trim(buf) /= "x") error stop

print *, "ok"
end program
