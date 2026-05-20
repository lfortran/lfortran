program file_68
! Test reading after an empty unformatted record
implicit none
integer :: a, x, y

open(10, file="file_68_test.dat", form="unformatted", status="replace")
write(10)        ! empty record
a = 42
write(10) a      ! record with one integer
a = 99
write(10) a      ! another record
close(10)

open(10, file="file_68_test.dat", form="unformatted", status="old")
read(10)         ! skip the empty record
read(10) x       ! read the integer
read(10) y       ! read next integer
close(10, status="delete")

if (x /= 42) error stop
if (y /= 99) error stop
print *, x, y
end program
