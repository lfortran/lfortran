module assumed_rank_22_mod
implicit none
contains
    subroutine read_real(iunit, x)
        integer, intent(in) :: iunit
        real(8), intent(inout) :: x(..)
        select rank (x)
        rank (1)
            read(iunit, *) x
        rank (2)
            read(iunit, *) x
        rank (3)
            read(iunit, *) x
        rank default
            error stop "unexpected rank"
        end select
    end subroutine
end module

program assumed_rank_22
use assumed_rank_22_mod
implicit none
real(8) :: a1(5), a2(5,2), w(3,3), a3(3,3,3), c(3)
integer :: u

open(newunit=u, file="assumed_rank_22_data.txt", status="replace", action="readwrite")
write(u, *) 100.0d0, 200.0d0, 300.0d0
rewind(u)
a1 = -1.0d0
call read_real(u, a1(1:5:2))
close(u, status="delete")
if (any(a1 /= [100.0d0, -1.0d0, 200.0d0, -1.0d0, 300.0d0])) then
    print *, a1
    error stop "rank-1 strided read failed"
end if

open(newunit=u, file="assumed_rank_22_data.txt", status="replace", action="readwrite")
write(u, *) 10.0d0, 20.0d0, 30.0d0
rewind(u)
a2 = -1.0d0
call read_real(u, a2(1:5:2, 2:2))
close(u, status="delete")
if (a2(1,2) /= 10.0d0 .or. a2(3,2) /= 20.0d0 .or. a2(5,2) /= 30.0d0) then
    print *, a2
    error stop "rank-2 strided read failed"
end if
if (any(a2(:,1) /= -1.0d0) .or. a2(2,2) /= -1.0d0 .or. a2(4,2) /= -1.0d0) then
    print *, a2
    error stop "rank-2 strided read overwrote wrong elements"
end if

open(newunit=u, file="assumed_rank_22_data.txt", status="replace", action="readwrite")
write(u, *) 10.0d0, 20.0d0, 30.0d0, 40.0d0
rewind(u)
w = -1.0d0
call read_real(u, w(1:3:2, 1:3:2))
close(u, status="delete")
if (w(1,1) /= 10.0d0 .or. w(3,1) /= 20.0d0 .or. &
        w(1,3) /= 30.0d0 .or. w(3,3) /= 40.0d0) then
    print *, w
    error stop "rank-2 two-dimension strided read failed"
end if
w(1:3:2, 1:3:2) = -1.0d0
if (any(w /= -1.0d0)) then
    print *, w
    error stop "rank-2 two-dimension strided read overwrote wrong elements"
end if

open(newunit=u, file="assumed_rank_22_data.txt", status="replace", action="readwrite")
write(u, *) 50.0d0, 60.0d0, 70.0d0, 80.0d0
rewind(u)
w = -1.0d0
call read_real(u, w(3:1:-2, 3:1:-2))
close(u, status="delete")
if (w(3,3) /= 50.0d0 .or. w(1,3) /= 60.0d0 .or. &
        w(3,1) /= 70.0d0 .or. w(1,1) /= 80.0d0) then
    print *, w
    error stop "rank-2 negative-stride read failed"
end if
w(3:1:-2, 3:1:-2) = -1.0d0
if (any(w /= -1.0d0)) then
    print *, w
    error stop "rank-2 negative-stride read overwrote wrong elements"
end if

open(newunit=u, file="assumed_rank_22_data.txt", status="replace", action="readwrite")
write(u, *) 11.0d0, 12.0d0, 13.0d0, 14.0d0, 15.0d0, 16.0d0, 17.0d0, 18.0d0
rewind(u)
a3 = -1.0d0
call read_real(u, a3(1:3:2, 1:3:2, 1:3:2))
close(u, status="delete")
if (a3(1,1,1) /= 11.0d0 .or. a3(3,1,1) /= 12.0d0 .or. &
        a3(1,3,1) /= 13.0d0 .or. a3(3,3,1) /= 14.0d0 .or. &
        a3(1,1,3) /= 15.0d0 .or. a3(3,1,3) /= 16.0d0 .or. &
        a3(1,3,3) /= 17.0d0 .or. a3(3,3,3) /= 18.0d0) then
    print *, a3
    error stop "rank-3 strided read failed"
end if
a3(1:3:2, 1:3:2, 1:3:2) = -1.0d0
if (any(a3 /= -1.0d0)) then
    print *, a3
    error stop "rank-3 strided read overwrote wrong elements"
end if

open(newunit=u, file="assumed_rank_22_data.txt", status="replace", action="readwrite")
write(u, *) 77.0d0
rewind(u)
a1 = -1.0d0
call read_real(u, a1(1:0))
close(u, status="delete")
if (any(a1 /= -1.0d0)) then
    print *, a1
    error stop "zero-size assumed-rank read failed"
end if

open(newunit=u, file="assumed_rank_22_data.txt", status="replace", action="readwrite")
write(u, *) 1.0d0, 2.0d0, 3.0d0
rewind(u)
c = -1.0d0
call read_real(u, c)
close(u, status="delete")
if (any(c /= [1.0d0, 2.0d0, 3.0d0])) then
    print *, c
    error stop "contiguous assumed-rank read failed"
end if
end program
