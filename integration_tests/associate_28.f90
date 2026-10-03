module associate_28_mod

   abstract interface
      subroutine factory()
      end subroutine factory
   end interface

contains

   subroutine client(val)
      procedure(factory), pointer, intent(out) :: val
      val => null()
   end subroutine client

end module associate_28_mod

program associate_28
   use associate_28_mod
   implicit none
   procedure(factory), pointer :: p
   call client(p)
   if (associated(p)) error stop
   print *, "ok"
end program associate_28
