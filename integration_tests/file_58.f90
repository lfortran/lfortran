program file_58
! Test unformatted stream write/read of logical(int8) arrays
use iso_fortran_env, only: int8
implicit none
logical(int8) :: a(3), c(3)
integer(int8) :: b(3)
integer :: u

a(1) = .true.
a(2) = .true.
a(3) = .true.

open(newunit=u, file='file_58.bin', access='stream', form='unformatted', &
     status='replace')
write(u) a
close(u)

b = 0
open(newunit=u, file='file_58.bin', access='stream', form='unformatted', &
     status='old')
read(u) b
close(u)

if (b(1) /= 1 .or. b(2) /= 1 .or. b(3) /= 1) error stop

c = .false.
open(newunit=u, file='file_58.bin', access='stream', form='unformatted', &
     status='old')
read(u) c
close(u, status='delete')

if (.not. c(1) .or. .not. c(2) .or. .not. c(3)) error stop

print *, "PASS"
end program
