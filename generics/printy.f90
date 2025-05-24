module real64_module

    use, intrinsic :: iso_fortran_env, only: real64
 
    abstract interface :: IPrintable
       subroutine output()
       end subroutine output
    end interface IPrintable
 
    implements IPrintable :: real(real64)
       procedure, pass :: output
    end implements real(real64)
 
 contains
 
    subroutine output(self)
       real(real64), intent(in) :: self
       write(*,*) "I am ", self
    end subroutine output
 
 end module real64_module
 
 program printy
 
    use, intrinsic :: iso_fortran_env, only: real64
 
    real(real64) :: y
 
    y = 4.9d0
    call y%output()
    
 end program printy