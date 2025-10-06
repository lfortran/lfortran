module string_76_mod
  implicit none

contains

character(5) function tolower (c) result (res)
  character, intent(in) :: c

  res = merge (achar (iachar (c) + 32), c, c >= 'A' .and. c <= 'Z') // "...."

end function

end module

program string_76
    use string_76_mod
    implicit none

    if (tolower("A") /= "a....") error stop

end program string_76
