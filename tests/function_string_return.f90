module function_string_return_mod

    contains
    function ff() result(r)
      character(10) :: r
      r = "Hello12345"
    end function
    
end module
  
  
  
program function_string_return
    use function_string_return_mod
    character(10) :: my_string
    my_string = ff()
    print *, my_string
end program 