module A
implicit none

contains

subroutine f(argument, is_allocated)
character(len=:), allocatable, intent(inout) :: argument
logical, intent(out) :: is_allocated
call g()
is_allocated = allocated(argument)

contains

subroutine g()
allocate(character(len=10) :: argument)

end subroutine

end subroutine

end module

program allocate_10
use A
implicit none

character(len=:), allocatable :: allocA
logical :: is_allocated_allocA

is_allocated_allocA = .false.
call f(allocA, is_allocated_allocA)
print *, is_allocated_allocA
if( .not. is_allocated_allocA ) error stop

end program
