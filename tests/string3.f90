module string3_mod
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
    end subroutine
  
end module
  
program string3
use string3_mod
integer :: x
character(10) :: char
x = 10
call foo_sub(x, char)
end program 
  
  
  