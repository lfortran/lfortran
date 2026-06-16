program associate_54
   implicit none

   type :: t
      integer :: id = 0
   end type t

   integer, target :: ints(2)
   integer, pointer :: pi => null()
   integer, pointer :: pj => null()

   integer, allocatable, target :: alloc_ints(:)
   integer, pointer :: pa => null()

   type(t), target :: tobj
   integer, pointer :: pm => null()

   ints(1) = 11
   ints(2) = 22
   pi => ints(1)
   pj => ints(2)
   if (.not. associated(pi, ints(1))) error stop 1
   if (associated(pi, ints(2))) error stop 2
   if (.not. associated(pj, ints(2))) error stop 3

   allocate(alloc_ints(3))
   alloc_ints = [100, 200, 300]
   pa => alloc_ints(2)
   if (.not. associated(pa, alloc_ints(2))) error stop 4
   if (associated(pa, alloc_ints(1))) error stop 5

   tobj%id = 42
   pm => tobj%id
   if (.not. associated(pm, tobj%id)) error stop 6
end program associate_54
