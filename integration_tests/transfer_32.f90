program transfer_32
! transfer() with CHARACTER on the array side: the byte offset of a result
! element inside the source is not an element index of the source, so the
! result must not be computed element by element from byte 0.
implicit none
integer :: a(3), b(3), i
integer, allocatable :: c(:)
character(12) :: buf
character(1) :: c1(12)
character(4) :: c4(3)

a = [11, 22, 33]

! integer array -> character scalar
buf = transfer(a, buf)
do i = 1, 3
    if (iachar(buf(4*i-3:4*i-3)) /= a(i)) error stop "int array -> char scalar"
    if (iachar(buf(4*i-2:4*i-2)) /= 0) error stop "int array -> char scalar pad"
end do

! character scalar -> integer array
b = 0
b = transfer(buf, b)
if (any(b /= a)) error stop "char scalar -> int array"

! character scalar -> integer array, with SIZE=
b = 0
b = transfer(buf, b, 3)
if (any(b /= a)) error stop "char scalar -> int array with size"

! character scalar -> allocatable integer array
allocate(c(3))
c = 0
c = transfer(buf, c)
if (any(c /= a)) error stop "char scalar -> allocatable int array"
deallocate(c)

! integer array -> character(1) array mold
c1 = achar(7)
c1 = transfer(a, c1)
do i = 1, 3
    if (iachar(c1(4*i-3)) /= a(i)) error stop "int array -> char(1) array"
    if (iachar(c1(4*i-2)) /= 0) error stop "int array -> char(1) array pad"
end do

! integer array -> character(1) array mold, with SIZE=
c1 = achar(7)
c1 = transfer(a, c1, 12)
do i = 1, 3
    if (iachar(c1(4*i-3)) /= a(i)) error stop "int array -> char(1) array size"
end do

! integer array -> character(4) array mold, and back again
c4 = "    "
c4 = transfer(a, c4)
do i = 1, 3
    if (transfer(c4(i), 0) /= a(i)) error stop "int array -> char(4) array"
end do

print *, b, iachar(c1(1)), iachar(c1(5)), iachar(c1(9))
end program
