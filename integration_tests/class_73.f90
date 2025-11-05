program class_73
   implicit none

   type, abstract :: toml_map_structure
   end type toml_map_structure

   type, extends(toml_map_structure) :: toml_ordered_map
      integer, allocatable :: arr(:)
   end type toml_ordered_map

   class(toml_map_structure), allocatable :: self
   type(toml_ordered_map), allocatable, target :: map
   type(toml_ordered_map), pointer :: map_ptr

   allocate(map)
   allocate(map%arr(3))
   map%arr = [1, 2, 3]    
   self = map
   select type(self)
    type is (toml_ordered_map)
        if (any(self%arr /= [1,2,3])) error stop
    class default
        error stop
   end select

   map_ptr => map
   if (any(map_ptr%arr /= [1,2,3])) error stop
   deallocate(self)
   self = map_ptr
   select type(self)
    type is (toml_ordered_map)
        if (any(self%arr /= [1,2,3])) error stop
    class default
        error stop
   end select

   deallocate(self)
   allocate(self)
   self = map
   select type(self)
    type is (toml_ordered_map)
        if (any(self%arr /= [1,2,3])) error stop
    class default
        error stop
   end select

   deallocate(self)
   call move_alloc(map, self)
   select type(self)
    type is (toml_ordered_map)
        if (any(self%arr /= [1,2,3])) error stop
    class default
        error stop
   end select
end program class_73
