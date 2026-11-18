program derived_types_261
implicit none
type :: crate
  character(len=3), pointer :: tags(:)
end type
type(crate) :: c, d
integer :: ierr

allocate(c%tags(3))
if (size(c%tags) /= 3) error stop
c%tags = ["abc", "def", "ghi"]
if (c%tags(2) /= "def") error stop
deallocate(c%tags)

allocate(d%tags(2), stat=ierr)
if (ierr /= 0) error stop
if (size(d%tags) /= 2) error stop
d%tags(1) = "xyz"
if (d%tags(1) /= "xyz") error stop
deallocate(d%tags)
print *, "OK"
end program
