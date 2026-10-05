program print6
! A list-directed output list that is not empty in the source but expands
! to no items at run time writes an empty record. The leading blank of
! list-directed output under --std=f23 is written only when at least one
! item is written.
implicit none
integer :: a(0), b(3), i
integer, allocatable :: c(:)
character(len=3) :: s(0)
b = [1, 2, 3]
allocate(c(0))
print *, a
print *, (i, i = 1, 0)
print *, c, b(2:1), s
print *, a, "x", s
print *, a, .true.
write(*, *) a
write(*, *) (i, i = 1, 0)
write(*, *) a, "y"
print *, "end"
end program
