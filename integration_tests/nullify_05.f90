program nullify_05
   procedure(OBJ), pointer :: calfun
   nullify(calfun)

   if (associated(calfun)) error stop

contains

   subroutine OBJ()
   end subroutine
end program
