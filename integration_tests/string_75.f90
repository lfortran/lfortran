module string_75_mod
  implicit none

contains

character function tolower (c) result (res)
  character, intent(in) :: c

  res = merge (achar (iachar (c) + 32), c, c >= 'A' .and. c <= 'Z')

end function

end module

program string_75
    use string_75_mod
    implicit none

    if (tolower("A") /= "a") error stop

end program string_75
