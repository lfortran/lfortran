! Test: Allocatable character array inside class member via select type
! Verifies that string descriptors for character arrays in heap-allocated
! struct members are correctly heap-allocated (not stack-allocated).
module select_type_45_types
implicit none
type :: t
    character(len=5), allocatable :: name(:)
end type
contains
subroutine setup(x, names)
    class(t), allocatable, intent(out) :: x
    character(len=5), allocatable, intent(in) :: names(:)
    allocate(t :: x)
    select type(x)
    type is (t)
        x%name = names
    end select
end subroutine
end module

program select_type_45
use select_type_45_types
implicit none
integer :: i
class(t), allocatable :: x
character(len=5), allocatable :: names(:)

allocate(names(3))
names(1) = 'aaa'
names(2) = 'bbb'
names(3) = 'ccc'
call setup(x, names)

select type(x)
type is (t)
    if (size(x%name) /= 3) error stop
    if (trim(x%name(1)) /= 'aaa') error stop
    if (trim(x%name(2)) /= 'bbb') error stop
    if (trim(x%name(3)) /= 'ccc') error stop
    do i = 1, 3
        print *, trim(x%name(i))
    end do
end select
end program
