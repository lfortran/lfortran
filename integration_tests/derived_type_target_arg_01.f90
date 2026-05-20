module derived_type_target_arg_01_mod
   implicit none
   type :: t
      integer :: id = -1
      type(t), pointer :: left => null()
   end type
   type :: ptr_t
      type(t), pointer :: array(:,:) => null()
   end type
contains
   function build_node(a) result(c)
      class(t), intent(in), target :: a
      type(t), pointer :: c
      allocate(c)
      c%id = a%id
      c%left => a
   end function

   function wrapper(list, i, j) result(c)
      type(ptr_t), dimension(:), intent(in) :: list
      integer, intent(in) :: i, j
      type(t), pointer :: c
      c => build_node(list(1)%array(i,j))
   end function
end module

program derived_type_target_arg_01
   use derived_type_target_arg_01_mod
   implicit none
   type(t), target :: x(1,1)
   type(ptr_t) :: list(1)
   type(t), pointer :: z
   integer :: id_a, id_b

   x(1,1)%id = 42
   list(1)%array => x

   z => wrapper(list, 1, 1)
   id_a = z%left%id
   id_b = z%left%id
   if (id_a /= 42) error stop "first read does not see actual arg"
   if (id_b /= 42) error stop "second read does not see actual arg"
   deallocate(z)
end program
