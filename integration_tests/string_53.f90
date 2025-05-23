module string_53_mod
    contains 
    
    pure function doubler(x) result (ret)
      integer,intent(in) :: x
      integer :: ret 
      ret = x * 2
    end function 
    
    subroutine sub()
      character(len=doubler(10)) :: zz
      print *, len(zz)
      ! Make sure character's length is set correctly when it's a call to a function
      if(len(zz) /= 20) error stop
    end subroutine
end module
  
program string_53
    use string_53_mod
    call sub()
end program 
  