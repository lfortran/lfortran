module string_65_mod
  
  contains
  
    function foo(probs) result(ret)
      logical :: ret
      character(len=64), intent(in), optional :: probs(:)
      if(present(probs)) then
        ret = all(probs == "HelloWorld!")
      else
        ret = .false.
      end if
    
    end function
  
end module 
  
  
program test
    use string_65_mod
    character(64) :: char_arr(10)
    if(foo() .neqv. .false.) error stop
    char_arr = "HelloWorld!"
    if(foo(char_arr) .neqv. .true.) error stop
    char_arr = "whatever"
    if(foo(char_arr) .neqv. .false.) error stop

end program test