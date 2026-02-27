module mmm
  contains 
  integer function dummy() result(r)
    r = 42
  end function dummy
  
  subroutine sub_test()
    print *, 'sub_test called'
  end subroutine
end module 

program p
  use mmm
  implicit none
  ! Error: assigning function to subroutine pointer
  procedure(sub_test), pointer :: pf
  pf => dummy
end program
