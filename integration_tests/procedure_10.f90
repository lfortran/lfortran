module main_module
contains
    subroutine sub_type(int)
      integer,intent(in) :: int
   end subroutine
   subroutine sub(int)
      integer,intent(in) :: int
      print *, int
      if (int /= 6) error stop
   end subroutine sub
end module main_module

program main
   use main_module
   procedure(sub_type), pointer :: p_sub
   p_sub => sub
   call p_sub(6)
end program main