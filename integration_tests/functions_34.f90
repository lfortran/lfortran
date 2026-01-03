module functions_34_mod_1 
    abstract interface
        subroutine OBJ()
        end subroutine 
    end interface

    procedure(OBJ), pointer :: f_ptr

contains
    subroutine caller_f_ptr()
        implicit none
        call f_ptr()
    end subroutine 

end module
    
    
module functions_34_mod_2

contains
    subroutine set_ptr_and_call()
        use functions_34_mod_1, only : f_ptr, caller_f_ptr 
        f_ptr => fff
        call caller_f_ptr()
    end subroutine 

    subroutine fff()
        print *, "hi"
    end subroutine 
    subroutine fff2()
        print *, "bye"
    end subroutine 

end module functions_34_mod_2
    
program functions_34
    use functions_34_mod_2
    use functions_34_mod_1 ,only: f_ptr
    call set_ptr_and_call()
    
    f_ptr => fff2
    call f_ptr()
end program functions_34