module module_string_57

   type :: MyType
      character(:), allocatable :: string
   contains
      procedure :: method
   end type MyType

contains
   
subroutine method(self, v)
class(MyType), intent(in)  :: self
character(len=*), intent(inout) :: v
select case (self%string)
case ('String1')
print *, 'Method called with String1'
v = 'String1 processed'
case ('String2')
print *, 'Method called with String2'
v = 'String2 processed'
case default
print *, 'Method called with unknown string'
v = 'Unknown string processed'
end select
end subroutine method
   
end module module_string_57

program string_57
use module_string_57
implicit none

type(MyType) :: obj
character(len=20) :: result
obj%string = 'String1'

call obj%method(result)
print *, 'Result after String1:', result
if (trim(result) /= 'String1 processed') error stop

obj%string = 'String2'
call obj%method(result)
print *, 'Result after String2:', result
if (trim(result) /= 'String2 processed') error stop

print *, 'Program completed successfully.'
end program
