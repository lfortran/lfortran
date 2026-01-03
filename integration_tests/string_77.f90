! Mainly testing declaration in function
module string_77_mod

    contains
  
  pure function foo (x) result(res)
    character(*), intent(in) :: x
    character(:), allocatable :: res
    allocate(character(len(x) + len("abc")):: res)
    res = x // 'abc'
  end function 
  
  pure function boo(x,y) result(res)
    integer, intent(in) :: x,y
    integer :: res
    res = x + y
  end function
  
  function ff(line,length) result(strout)  
    character(len=*):: line
    integer :: length
    character(len=boo(length, len(foo(line)))) :: strout ! the main concern of this test -- Is to handle this declaration correctly
  end function 
  
end module 
  
  
program string_77
    use string_77_mod
    print *, len(ff("Hello", 10)) !! Expected output: 18
    if(len(ff("Hello", 10)) /= 18) error stop
end program 