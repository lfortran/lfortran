! FORALL with array constructors: a constructor that depends on the FORALL
! indices is evaluated for its own index values, and one that does not is
! still evaluated correctly.
program forall_11
implicit none
type :: st
    integer :: arr(2)
end type
type :: alloc_st
    integer, allocatable :: arr(:)
end type
type(st) :: s(3)
type(alloc_st) :: t(3), t0(3)
integer :: x(3), y(3,4), m(3,2,2), i, j

x = 0
forall (i = 1:3) x(i:i) = [i]
if (any(x /= [1, 2, 3])) error stop 1

forall (i = 1:3) s(i)%arr = [i, 10*i]
do i = 1, 3
    if (any(s(i)%arr /= [i, 10*i])) error stop 2
end do

do i = 1, 3
    allocate(t(i)%arr(2))
end do
forall (i = 1:3) t(i)%arr = [i, 10*i]
do i = 1, 3
    if (any(t(i)%arr /= [i, 10*i])) error stop 3
end do

forall (i = 1:3) y(i,:) = [(i*j, j = 1, 4)]
do i = 1, 3
    if (any(y(i,:) /= [(i*j, j = 1, 4)])) error stop 4
end do

y = 0
forall (i = 1:3, j = 1:4, i /= j) y(i,j:j) = [i + 10*j]
do j = 1, 4
    do i = 1, 3
        if (i /= j .and. y(i,j) /= i + 10*j) error stop 5
        if (i == j .and. y(i,j) /= 0) error stop 6
    end do
end do

x = [1, 2, 3]
forall (i = 1:3) x(i:i) = [x(4-i)]
if (any(x /= [3, 2, 1])) error stop 7

forall (i = 1:3) m(i,:,:) = reshape([(j, j = 1, 4)], [2, 2])
do i = 1, 3
    if (any(m(i,:,1) /= [1, 2]) .or. any(m(i,:,2) /= [3, 4])) error stop 8
end do

do i = 1, 3
    allocate(t0(i)%arr(0:1))
end do
forall (i = 1:3) t0(i)%arr = [i, 10*i]
do i = 1, 3
    if (any(t0(i)%arr /= [i, 10*i])) error stop 9
end do

print *, "ok"
end program
