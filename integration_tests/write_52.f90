program write_52
implicit none
type rk_comm
    character(len=8) :: rec(4)
end type
character(len=4) :: rec(4)
type(rk_comm) :: comm
integer :: n

rec = '-'
write (rec(2:3), '(a/a)') 'u', 'v'
print '(4(a,"|"))', rec
if (rec(1) /= '-' .or. rec(2) /= 'u' .or. rec(3) /= 'v' .or. rec(4) /= '-') error stop

n = 4
comm%rec = '-'
write (comm%rec(n-1:n), '(a/a)') 'x', 'y'
print '(4(a,"|"))', comm%rec
if (comm%rec(2) /= '-' .or. comm%rec(3) /= 'x' .or. comm%rec(4) /= 'y') error stop
end program
