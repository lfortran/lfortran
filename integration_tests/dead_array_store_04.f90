! An inquiry about a section of an array (`ubound`, `size`, `shape`)
! evaluates the section's bounds, so a bound that references the array
! reads it: the dead_array_store pass keeps the assignment before it.
program dead_array_store_04
implicit none
integer :: x(4), n, k(1)

x = 3
n = ubound(x(x(1):4), 1)
x = 5
if (n /= 2) error stop 1

x = 2
n = ubound(x(1:x(2)), 1)
x = 5
if (n /= 2) error stop 2

x = 3
n = size(x(x(1):4))
x = 5
if (n /= 2) error stop 3

x = 3
k = shape(x(x(1):4))
x = 5
if (k(1) /= 2) error stop 4

x = 1
n = ubound(x(1:4:x(3)), 1)
x = 5
if (n /= 4) error stop 5
print *, "ok"
end program
