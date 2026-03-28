subroutine sub(s) bind(C)
  implicit none
  character(len=:), allocatable :: s
  s = "hello"
end subroutine

program bindc_18
  implicit none
  interface
    subroutine sub(s) bind(C)
      implicit none
      character(len=:), allocatable :: s
    end subroutine
  end interface
  character(len=:), allocatable :: e
  call sub(e)
  print *, e
  if (len(e) /= 5) error stop
  if (e /= "hello") error stop
end program
