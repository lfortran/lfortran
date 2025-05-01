

! Test circular dependencies when declaring interface and using it in the function constructing that same interface


module functions_36_mod
    real :: var
    interface generic  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Depends on both `f1` and `f2` to be declared 
      procedure :: f1
      procedure :: f2
    end interface
    
    contains
    
    pure function f1(vv) result(length)
    real, intent(in) :: vv
    integer :: rr(generic(vv, 2)) !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Depends on `f2` (but through interface) to be fully declared
    integer :: length
    length = size(rr)
    end function 
    
    pure function f2(rr, int) result(length)
    real, intent(in) :: rr
    integer, intent(in) :: int
    integer :: length
    length = int*2
    end function 
    
    subroutine maybe()
      character(10) :: string(generic(var)) 
      print *, size(string)
      if (size(string) /= 4) error stop
    end subroutine maybe
    
end module 
    
    
program functions_36
    use functions_36_mod
    var =1.5
    call maybe()
end program 
    
    
    
    