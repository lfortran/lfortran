program allocate_26
implicit none
integer(4) :: ind
character(len=:, kind=1), allocatable, dimension(:) :: arr
integer(4) :: i
allocate(arr(3), mold="hello")
arr = "hello"
print *, arr
if (any(arr /= ["hello", "hello", "hello"])) error stop
end program allocate_26