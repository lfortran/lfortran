! Test StringToArray casting
program string_87
    character(10) :: str
    character(10) :: str2
    
    str = "HelloWorld"
    call ff1(str)
    print *, str
    if(str /= "HelloWTrld") error stop 3
    
    str = "HelloWorld"
    call ff2(str)
    print *, str
    if(str /= "HelloWTrld") error stop 6
    
    str = "HelloWorld"
    call ff3(str)
    print *, str
    if(str /= "HelloWTrld") error stop 9
    
    str = "HelloWorld"
    call ff4(str)
    print *, str
    if(str /= "HelloWTrld") error stop 12
    
    ! TODO :: SUPPORT CASE BELOW

    ! str = "HelloWorld"
    ! call ff5(str, 2)
    ! print *, str
    ! if(str /= "HelloWTrld") error stop 15
    
    str = "Hello"
    call ff1_1(trim(str)//"World")

    contains
    
    subroutine ff1(s)
      character(5) :: s(2)
      if(s(1) /= "Hello") error stop 1
      if(s(2) /= "World") error stop 2
      s(2)(2:2) = "T"
    end subroutine

    subroutine ff2(s)
      character(5) :: s(*)
      if(s(1) /= "Hello") error stop 4
      if(s(2) /= "World") error stop 5
      s(2)(2:2) = "T"
    end subroutine

    subroutine ff3(s)
      character(*) :: s(1)
      if(len(s(1)) /= 10) error stop 7
      if(s(1) /= "HelloWorld") error stop 8
      s(1)(7:7) = "T"
    end subroutine

    subroutine ff4(s)
      character(*) :: s(*)
      if(len(s(1)) /= 10) error stop 10
      if(s(1) /= "HelloWorld") error stop 11
      s(1)(7:7) = "T"
    end subroutine

    subroutine ff5(s, n)
      integer :: n
      character(*) :: s(n)
      if(len(s(1)) /= 10) error stop 13
      if(s(1) /= "HelloWorld") error stop 14
      s(1)(7:7) = "T"
    end subroutine

    subroutine ff1_1(s)
      character(2) :: s(5)
      if(s(1) /= "He") error stop 16
      if(s(2) /= "ll") error stop 17
      if(s(3) /= "oW") error stop 18
      if(s(4) /= "or") error stop 19
      if(s(5) /= "ld") error stop 20
    end subroutine

end program 