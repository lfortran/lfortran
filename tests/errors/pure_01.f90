program pure_01
  implicit none
  call sub(666)
contains
  pure subroutine sub(b)
    integer,intent(in)::b
    print "(A,I0)",'b = ',b
  end subroutine sub
end program pure_01