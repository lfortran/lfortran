module string_48_mod
   implicit none

contains

   subroutine split(s)
      character(len=:), allocatable, intent(out) :: s(:)
      character(:), allocatable :: tstr

      allocate(character(len=1) :: s(1))

      tstr = "A"
      s(1) = tstr
   end subroutine split

end module string_48_mod

program string_48
   use string_48_mod
   implicit none
   character(:), allocatable :: out(:)

   call split(out)

   print *, "out ", out(1)
   if (out(1) /= "A") error stop

end program string_48

