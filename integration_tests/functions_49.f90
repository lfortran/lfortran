module functions_49_mod
    contains
    function foo (x) result(res)
      character(*) :: x
      character(10) :: res
    end function 
  
  function ff(line,length) result(strout)  
    character(len=*):: line
    integer :: length
    character(len=max(length, len(foo(line)))) :: strout
  end function 
  
  end module 
  
  
  program functions_49
    use functions_49_mod
    integer :: x
    x = len(ff("hello",5))
    print *, x
    if(x /= max(5, len(foo("hello")))) error stop
  end program 