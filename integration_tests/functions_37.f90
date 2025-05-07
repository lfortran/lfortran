module functions_37_mod
    interface generic
       procedure :: ff
    end interface
  
    contains
  
    pure function ff() result(ret)
      integer :: ret
      ret  = 190
    end function
  
    function foo() result(arr) 
      integer :: arr(generic()) ! Depends on `ff` to be fully declared (through interface `generic`)
    end function
  
end module
  
program functions_37
use functions_37_mod
print *, size(foo())
if(size(foo()) /= 190) error stop
end program functions_37