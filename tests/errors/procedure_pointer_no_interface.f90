module mmm
  contains 
  integer function dummy() result(r)
    r = 42
  end function dummy
end module 

program p
  use mmm
  implicit none
  ! Error: procedure() without explicit interface
  procedure(), pointer :: pf
  pf => dummy
end program
