! FORALL that reallocates an allocatable component to the value of an array
! constructor whose size depends on the FORALL index.
program forall_12
implicit none
type :: alloc_st
    integer, allocatable :: arr(:)
end type
type(alloc_st) :: u(3)
integer :: i, j

do i = 1, 3
    allocate(u(i)%arr(5))
end do
forall (i = 1:3) u(i)%arr = [(j*i, j = 1, i)]
do i = 1, 3
    if (size(u(i)%arr) /= i) error stop 1
    if (any(u(i)%arr /= [(j*i, j = 1, i)])) error stop 2
end do

forall (i = 1:3) u(i)%arr = [(u(i)%arr(i+1-j), j = 1, i), 10*i]
do i = 1, 3
    if (size(u(i)%arr) /= i + 1) error stop 3
    if (any(u(i)%arr /= [((i+1-j)*i, j = 1, i), 10*i])) error stop 4
end do

print *, "ok"
end program
