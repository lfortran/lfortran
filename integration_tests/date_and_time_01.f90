program date_and_time_01
! VALUES may be larger than 8; only its first 8 elements are defined
implicit none
integer :: v(10), w(0:9), s(12)
integer(8) :: v8(10)
integer, allocatable :: a(:)
integer, pointer :: p(:)

v = -7
call date_and_time(values=v)
if (v(1) < 2000 .or. v(2) < 1 .or. v(2) > 12) error stop 1
if (any(v(9:10) /= -7)) error stop 2

w = -7
call date_and_time(values=w)
if (w(0) < 2000 .or. w(1) < 1 .or. w(1) > 12) error stop 3
if (any(w(8:9) /= -7)) error stop 4

v8 = -7
call date_and_time(values=v8)
if (v8(1) < 2000 .or. v8(2) < 1 .or. v8(2) > 12) error stop 5
if (any(v8(9:10) /= -7)) error stop 6

allocate(a(9))
a = -7
call date_and_time(values=a)
if (a(1) < 2000 .or. a(2) < 1 .or. a(2) > 12) error stop 7
if (a(9) /= -7) error stop 8

s = -7
call date_and_time(values=s(3:12))
if (s(3) < 2000 .or. s(4) < 1 .or. s(4) > 12) error stop 9
if (any(s(1:2) /= -7) .or. any(s(11:12) /= -7)) error stop 10

allocate(p(0:9))
p = -7
call date_and_time(values=p)
if (p(0) < 2000 .or. p(1) < 1 .or. p(1) > 12) error stop 11
if (any(p(8:9) /= -7)) error stop 12
deallocate(p)

deallocate(a)
allocate(a(-1:8))
a = -7
call date_and_time(values=a)
if (a(-1) < 2000 .or. a(0) < 1 .or. a(0) > 12) error stop 13
if (any(a(7:8) /= -7)) error stop 14

print *, "ok"
end program
