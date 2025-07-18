module string_62_mod
    character(3) :: str = "Hi!"
    contains
    subroutine ff
      print *,"Side 2 : ", str
      if(str /= "Bye") error stop
      str = "123"
      print *, ">> Side 2 changed variable value to:", str
    end subroutine
end module 
  