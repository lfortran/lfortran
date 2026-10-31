! Deferred-length character pointer initialized to a substring (#16039)
! (gfortran releases without PR 125763 ICE on this)
module pointer_init_substring_02_m
  implicit none
  character(len=5), target :: t = "abcde"
contains
  subroutine check()
    character(len=:), pointer :: pd1 => t(2:4)
    if (len(pd1) /= 3 .or. pd1 /= "bcd") error stop 1
  end subroutine
end module

program pointer_init_substring_02
  use pointer_init_substring_02_m
  call check()
  print *, "ok"
end program
