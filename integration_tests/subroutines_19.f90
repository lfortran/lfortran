program main
   procedure(OBJ), pointer :: proc_ptr
   call sub(proc_ptr)

contains
   subroutine OBJ()
      print *, "hello"
   end subroutine

   subroutine sub(calfun)
      procedure(OBJ), pointer , intent(inout) :: calfun
      calfun => OBJ
      call calfun
   end subroutine
end program