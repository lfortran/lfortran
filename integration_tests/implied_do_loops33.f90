program implied_do_loops33
implicit none
character(len=6) :: c(2)
character(len=100) :: buf
integer :: i, n, io_unit

c(1) = "ab"
c(2) = "cde"
io_unit = 10

! trim() with variable upper bound and A-width format (with sibling arg)
! Write to external file and read back to verify
n = 1
open(io_unit, file="implied_do_loops33.tmp", status="replace")
write (io_unit, "(2(1x,a10))") "x", (trim(c(i)), i=1,n)
close(io_unit)
open(io_unit, file="implied_do_loops33.tmp", status="old")
read (io_unit, "(a)") buf
close(io_unit, status="delete")
if (buf /= "          x         ab") error stop

! Single element, trim only (no sibling), variable bound
n = 1
open(io_unit, file="implied_do_loops33.tmp", status="replace")
write (io_unit, "(1x,a10)") (trim(c(i)), i=1,n)
close(io_unit)
open(io_unit, file="implied_do_loops33.tmp", status="old")
read (io_unit, "(a)") buf
close(io_unit, status="delete")
if (buf /= "         ab") error stop

! trim with trailing sibling arg, variable bound
n = 1
open(io_unit, file="implied_do_loops33.tmp", status="replace")
write (io_unit, "(2(1x,a10))") (trim(c(i)), i=1,n), "y"
close(io_unit)
open(io_unit, file="implied_do_loops33.tmp", status="old")
read (io_unit, "(a)") buf
close(io_unit, status="delete")
if (buf /= "         ab          y") error stop

print *, "PASS"
end program
