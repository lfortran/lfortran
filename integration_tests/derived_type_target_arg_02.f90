module derived_type_target_arg_02_mod
   implicit none
   type :: t
      integer :: id = -1
      type(t), pointer :: link => null()
   end type
   type :: container_t
      type(t), dimension(1) :: z
   end type
contains
   function inner(a) result(c)
      type(t), intent(in), target :: a
      type(t), pointer :: c
      allocate(c)
      c%id = 999
      c%link => a
   end function inner

   function outer(val) result(output)
      type(t), intent(in) :: val
      type(t), pointer :: output
      output => inner(val)
   end function outer
end module

program derived_type_target_arg_02
   use derived_type_target_arg_02_mod
   implicit none
   type(container_t) :: cont
   type(t), dimension(3) :: arr
   type(t), pointer :: r1, r2

   ! Case 1: struct-field actual (fixed-size array component selected by index)
   cont%z(1)%id = 42
   r1 => outer(cont%z(1))
   cont%z(1)%id = 100
   if (r1%link%id /= 100) error stop "r1%link must alias cont%z(1)"
   deallocate(r1)

   ! Case 2: plain array element actual (ArrayItem)
   arr(2)%id = 7
   r2 => outer(arr(2))
   arr(2)%id = 13
   if (r2%link%id /= 13) error stop "r2%link must alias arr(2)"
   deallocate(r2)

end program
