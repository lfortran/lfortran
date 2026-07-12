integer :: a(2)
open (10, file='test.txt', status='replace')
a = (/ 1, 2 /)
write (10, *) a
rewind (10)
a = (/ 0, 0 /)
read (10, *) a
if (any(a /= (/ 1, 2 /))) then
    error stop
end if
print *, "ok"
end