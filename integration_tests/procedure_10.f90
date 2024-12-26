module procedure_10_mod
contains
    subroutine sub_type(int)
      integer,intent(in) :: int
   end subroutine
   subroutine sub(int)
      integer,intent(in) :: int
      print *, int
      if (int /= 6) error stop
   end subroutine sub
end module procedure_10_mod

program procedure_10
   use procedure_10_mod
   procedure(sub_type), pointer :: p_sub
   p_sub => sub
   call p_sub(6)
end program procedure_10