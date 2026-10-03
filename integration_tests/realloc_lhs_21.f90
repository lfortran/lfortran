program realloc_lhs_21
implicit none
character(len=100), allocatable :: dst(:)
character(len=:), allocatable :: src(:)

allocate(character(len=10) :: src(1))
src(1) = "world"

! Assigning a deferred-length character array to a fixed-length allocatable
! character array should allocate dst with len=100, not len=10.
dst = src

if (len(dst(1)) /= 100) error stop
if (trim(dst(1)) /= "world") error stop
! Reading twice ensures no heap corruption from undersized allocation
if (trim(dst(1)) /= "world") error stop
print *, "PASSED"
end program realloc_lhs_21
