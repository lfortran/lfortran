program implied_do_loops31
implicit none
character(len=4) :: a(2)
character(len=100) :: buf
integer :: i

a(1) = "aa"
a(2) = "bb"

! Test: write with trim() in implied DO loop preserves preceding items
write (buf, "(3(1x,a))") "x", (trim(a(i)), i=1,2)
if (buf /= " x aa bb") error stop

! Test: write with trim() in implied DO loop preserves following items
write (buf, "(3(1x,a))") (trim(a(i)), i=1,2), "y"
if (buf /= " aa bb y") error stop

! Test: single item implied DO with trim() and preceding literal
write (buf, "(2(1x,a))") "z", (trim(a(i)), i=1,1)
if (buf /= " z aa") error stop

print *, "PASS"
end program implied_do_loops31
