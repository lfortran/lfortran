module string_54_mod
    interface double
      procedure :: double_
    end interface
    
    contains 
    pure function double_(x) result(ret)
      integer, intent(in) :: x
      integer :: ret
      ret = x*2
    end function
    
! Test correct FunctionType + FunctionParam  
    subroutine foo_sub(x, char)
      integer , intent(in) :: x 
      character(double(x)) :: char
      ! if(len(char) /= 20) error stop ! >>>>>>>>>>>>>>>>>>>>>>>>>>> Uncomment after string is refactored(char should be of physicalType `DescriptorString`).
    end subroutine
  
end module
  
program string_54
use string_54_mod
integer :: x
character(10) :: char
x = 10
call foo_sub(x, char)
end program 
  
  
  