module functions_33_mod 
    abstract interface
       subroutine interface_signature()
       end subroutine 
    end interface
    
    procedure(interface_signature), pointer :: func_ptr
 
    contains
       subroutine test_uobyqa()
          func_ptr => ff
             call func_ptr()
        end subroutine
 
        subroutine ff()
          print *, "hi"
        end subroutine 
    end module functions_33_mod
    
    
    
    program functions_33
      use functions_33_mod
      call test_uobyqa()
    end program 