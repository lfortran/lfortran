module mod_implicit_typing_06
   implicit none
   private
   public a
   integer, parameter :: a = 1
end module mod_implicit_typing_06

program implicit_typing_06 
    use mod_implicit_typing_06
    print *, a
    if (a /= 1) error stop
end program