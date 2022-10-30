program types_17
  implicit none

contains
  subroutine f1()
    integer, pointer :: ptr
    nullify(ptr)
  end subroutine

  subroutine f2()
    character(len=2), pointer :: ptr
    nullify(ptr)
  end subroutine
end program
