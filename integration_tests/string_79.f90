! Testing declaration in function
module string_79_mod
    contains
    pure function foo (x) result(res)
      character(*), intent(in)  :: x
      character(:), allocatable :: res
      allocate(character(len(x) + len("abc")):: res)
      res = x // 'abc'
    end function 
  
    pure function ff(line,length) result(strout)  
    character(len=*) , intent(in) :: line
    integer          , intent(in) :: length
    character(len=max(length, len(foo(line)))) :: strout ! The main concern of this test -- Is to handle this declaration correctly
  end function 
  
  end module 
  
  
  program string_79
    use string_79_mod
    print *, len(ff("HelloWorld", 10))
    if(len(ff("HelloWorld", 10)) /= 13) error stop
  end program 