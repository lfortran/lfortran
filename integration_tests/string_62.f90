program string_62
    use string_62_mod
    print *,"Side 1 : ", str
    if(str /="Hi!") error stop
  
    str = "Bye"
    print *, ">> Side 1 changed variable value to :", str
  
    call ff
    print *,"Side 1 : ", str
    if(str /="123") error stop
  end program 