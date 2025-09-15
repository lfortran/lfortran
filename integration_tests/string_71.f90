! Check that substrings are passed by reference.
module string_71_mod
    contains 
    subroutine s(str)
      character(1) :: str
      str = "M"
    end subroutine
end module
  
program string_71
    use string_71_mod
    character(5) :: str
    integer :: i
    i = 1
    str = "Hello"
    call s(str(i:i))
    print *, str
    if(str /= "Mello") error stop
end program