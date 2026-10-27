! A character pointer initialized to a (sub)string of a target (#16039)
module pointer_init_substring_01_m
  implicit none
  character(len=5), target :: t = "abcde"
contains
  subroutine check()
    character(len=3), pointer :: pe1 => t(2:4)
    character(len=5), pointer :: pf => t
    if (pe1 /= "bcd") error stop 2
    if (pf /= "abcde") error stop 3
  end subroutine
end module

program pointer_init_substring_01
  use pointer_init_substring_01_m
  implicit none
  character(len=5), target :: s = "hello"
  character(len=3), pointer :: p => s(2:4)
  if (p /= "ell") error stop 4
  call check()
  print *, "ok"
end program
