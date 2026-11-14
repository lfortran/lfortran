! An assumed-size array that is a dummy of an ENTRY, and not of the host
! procedure, is passed as a pointer. A call with an implicit interface
! passes that pointer; the entry has to read the same storage.
subroutine entry_34_driver(n, lower, upper, infin, infis, d, e, val)
implicit none
integer :: n, infin(*), infis
double precision :: lower(*), upper(*), d, e, val
double precision :: mvndnt
val = mvndnt(n, lower, upper, infin, infis, d, e)
end subroutine

double precision function mvndfn(n, w)
implicit none
integer :: n, infin(*), infis, infi(8)
double precision :: w(*), lower(*), upper(*), d, e, mvndnt
double precision :: a(8), b(8)
save a, b, infi
mvndfn = 1.0d0
return
entry mvndnt(n, lower, upper, infin, infis, d, e)
a(1) = lower(1)
b(1) = upper(1)
infi(1) = infin(1)
d = a(1)
e = b(1)
infis = infi(1)
mvndnt = a(1)
end function

program entry_34
implicit none
interface
    subroutine entry_34_driver(n, lower, upper, infin, infis, d, e, val)
    integer :: n, infin(*)
    double precision :: lower(*), upper(*), d, e, val
    integer :: infis
    end subroutine
end interface
double precision :: lower(1), upper(1), d, e, val
integer :: infin(1), n, infis
n = 1
lower(1) = 1.5d0
upper(1) = -0.126d0
infin(1) = 2
call entry_34_driver(n, lower, upper, infin, infis, d, e, val)
if (abs(val - 1.5d0) > 1.0d-12) error stop 1
if (abs(d - 1.5d0) > 1.0d-12) error stop 2
if (abs(e + 0.126d0) > 1.0d-12) error stop 3
if (infis /= 2) error stop 4
print *, val
end program
