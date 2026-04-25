program class_140
! Test associated() with two class(*) pointer array arguments
implicit none
class(*), pointer :: p1(:), p2(:), p3(:)
class(*), allocatable, target :: data1(:), data2(:)

allocate(data1, source=[1.0, 2.0])
allocate(data2, source=[3.0, 4.0])
p1 => data1
p2 => data1

! Both point to same target
if (.not. associated(p1, p2)) error stop

! p3 points to a different target
p3 => data2
if (associated(p1, p3)) error stop

print *, "PASS"
end program
