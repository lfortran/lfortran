integer :: a(2)
logical :: is_open
open (10, file='interactive_rewind_01.txt', status='replace')
a = (/ 1, 2 /)
write (10, *) a
rewind (10)
a = (/ 0, 0 /)
read (10, *) a
if (any(a /= (/ 1, 2 /))) then
    error stop
end if

! backspace: after reading the record, step back and read it again
backspace (10)
a = (/ 0, 0 /)
read (10, *) a
if (any(a /= (/ 1, 2 /))) then
    error stop
end if

! inquire: the unit should report as connected
inquire (unit=10, opened=is_open)
if (.not. is_open) then
    error stop
end if

! endfile: truncate the file at the current position
rewind (10)
write (10, *) a
endfile (10)

print *, "ok"
end