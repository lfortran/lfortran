subroutine print_generic(generic)
   class(*), intent(in), optional  :: generic
   select type(generic)
      type is (character(len=*))
         print*, adjustl(generic)
   end select
end subroutine print_generic

program main
end