program where_01
implicit none
integer :: a(10), b(10), i
a = -2
a(1) = 1
a(5) = 8
b = -3
where (a >= 0)
    b = 1
else where
    b = 0
end where

where (a >= 0)
    b = 1
elsewhere
    b = 0
end where

where (a >= 0)
    b = 1
elsewhere
    b = 0
endwhere

if (abs(b(1) - 1) > 1e-6) error stop
if (abs(b(5) - 1) > 1e-6) error stop
do i = 2, 4
    if (abs(b(i) - 0) > 1e-6) error stop
end do
do i = 6, 10
    if (abs(b(i) - 0) > 1e-6) error stop
end do
print *, b
print *, a
end program
