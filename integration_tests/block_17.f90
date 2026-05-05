! Test that nested vars inside a block (select-type-integer block) is properly nested
module block_17_mod

   contains
   subroutine containing_foo()
      class(*),allocatable :: dummy
      integer :: var_to_nest
      logical :: var_to_nest_2
      var_to_nest = 1
      var_to_nest_2 = .true.
      allocate(integer :: dummy)

      call foo(dummy)
      contains

      subroutine foo(in_var)
         class(*),intent(in) :: in_var
         select type(in_var)
            type is (integer)
               if(var_to_nest == 1 .and. var_to_nest_2) then
                  print *, "works properly"
               else 
                  error stop
               end if 
            class default
               error stop
            end select 
      end subroutine 

   end subroutine 
end module 

program block_17
   use block_17_mod
   call containing_foo
   
end program