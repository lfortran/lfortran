module M_CLI2_21

contains

subroutine get_args_fixed_length_a_array(strings)
character(len=*), allocatable :: strings(:)
character(len=:), allocatable :: strings_a(:)
integer :: place
   if(place > 0) then
      if (len(strings_a) <= len(strings))then
         strings = strings_a
      else
         strings = [character(len=len(strings)) ::]
      endif
   endif
   if(place > 0) then
        strings = [character(len=len(strings)) ::]
   endif
end subroutine get_args_fixed_length_a_array

end module M_CLI2_21

program modules_35
    print *, "executing modules_35"
end program
