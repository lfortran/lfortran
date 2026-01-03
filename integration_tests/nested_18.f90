! Checking Nested Variables Pass: Host variable from a subroutine 
! used in nested function, Passed through Do-While loop check & If block checks
module temp
  contains
    subroutine demo()
      implicit none
      integer :: a       ! Host variable
      a = 10
      do while(f())  ! While Loop access
        a = 5
      end do
      if (f2()) then ! If Block executes
        a = 10
      end if
    contains
      logical function f()
          character(len=5) :: str1
          write(str1, "(I0)") a
          if ((trim(str1)) /= "10") error stop
          print *, a, str1
          f = .false.
      end function f
      logical function f2()
          character(len=5) :: str1
          write(str1, "(I0)") a
          if ((trim(str1)) /= "10") error stop
          print *, a, str1
          f2 = .true.
      end function f2
    end subroutine demo 
end module

program nested_18
  use temp
  implicit none
  call demo()
  print *, "Nested variable test passed"
end program nested_18