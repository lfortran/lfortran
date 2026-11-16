program cshift_eoshift_shift_overflow_01
! Compile-time cshift/eoshift with |shift| > size(array) (#17875)
implicit none
integer, parameter :: a(4) = [1, 2, 3, 4]
real, parameter :: r(4) = [1.5, 2.5, 3.5, 4.5]
double precision, parameter :: d(3) = [1.0d0, 2.0d0, 3.0d0]
logical, parameter :: l(3) = [.true., .false., .false.]
character(len=2), parameter :: c(3) = ["ab", "cd", "ef"]
integer, parameter :: z(0) = [integer ::]

integer, parameter :: ca1(4) = cshift(a, 5)
integer, parameter :: ca2(4) = cshift(a, -6)
integer, parameter :: ca3(4) = cshift(a, 11)
integer, parameter :: ca4(4) = cshift(a, -9)
integer, parameter :: ca5(4) = cshift(a, 8)
real, parameter :: cr1(4) = cshift(r, 6)
real, parameter :: cr2(4) = cshift(r, -5)
double precision, parameter :: cd1(3) = cshift(d, 7)
logical, parameter :: cl1(3) = cshift(l, -4)
character(len=2), parameter :: cc1(3) = cshift(c, 5)
character(len=2), parameter :: cc2(3) = cshift(c, -7)

integer, parameter :: ea1(4) = eoshift(a, 6)
integer, parameter :: ea2(4) = eoshift(a, -6)
integer, parameter :: ea3(4) = eoshift(a, 9, boundary=7)
integer, parameter :: ea4(4) = eoshift(a, -5, boundary=-1)
real, parameter :: er1(4) = eoshift(r, 5)
real, parameter :: er2(4) = eoshift(r, -7, boundary=9.5)
double precision, parameter :: ed1(3) = eoshift(d, 4, boundary=2.5d0)
logical, parameter :: el1(3) = eoshift(l, -4)
logical, parameter :: el2(3) = eoshift(l, 5, boundary=.true.)
character(len=2), parameter :: ec1(3) = eoshift(c, 4)
character(len=2), parameter :: ec2(3) = eoshift(c, -6, boundary="zz")

integer, parameter :: cz(0) = cshift(z, 5)
integer, parameter :: ez(0) = eoshift(z, 6)

if (any(ca1 /= [2, 3, 4, 1])) error stop 1
if (any(ca2 /= [3, 4, 1, 2])) error stop 2
if (any(ca3 /= [4, 1, 2, 3])) error stop 3
if (any(ca4 /= [4, 1, 2, 3])) error stop 4
if (any(ca5 /= a)) error stop 5
if (any(cr1 /= [3.5, 4.5, 1.5, 2.5])) error stop 6
if (any(cr2 /= [4.5, 1.5, 2.5, 3.5])) error stop 7
if (any(cd1 /= [2.0d0, 3.0d0, 1.0d0])) error stop 8
if (any(cl1 .neqv. [.false., .true., .false.])) error stop 9
if (any(cc1 /= ["ef", "ab", "cd"])) error stop 10
if (any(cc2 /= ["ef", "ab", "cd"])) error stop 11

if (any(ea1 /= 0)) error stop 12
if (any(ea2 /= 0)) error stop 13
if (any(ea3 /= 7)) error stop 14
if (any(ea4 /= -1)) error stop 15
if (any(er1 /= 0.0)) error stop 16
if (any(er2 /= 9.5)) error stop 17
if (any(ed1 /= 2.5d0)) error stop 18
if (any(el1)) error stop 19
if (.not. all(el2)) error stop 20
if (any(ec1 /= "  ")) error stop 21
if (any(ec2 /= "zz")) error stop 22

if (size(cz) /= 0) error stop 23
if (size(ez) /= 0) error stop 24
print *, "ok"
end program
