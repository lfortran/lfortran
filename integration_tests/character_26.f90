module module_character_26
    use iso_c_binding
    implicit none
    character(len=*), parameter :: msg = "test"
  contains
    subroutine sub(stat, errmsg) bind(C)
      integer(c_int), intent(out) :: stat
      character(len=:), intent(out), allocatable :: errmsg
      stat = 0
      errmsg = msg
    end subroutine
end module module_character_26
  
program character_26
    use module_character_26
    character(len=:), allocatable :: s
    integer(c_int) :: i
    call sub(i, s)
    if (s /= "test") error stop
    deallocate(s)
end program character_26