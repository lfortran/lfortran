module derived_types_46_m
   type :: derived
       integer :: n
   end type derived

   interface derived
      procedure :: init
   end interface derived
contains

   function init() result(self)
      type(derived) :: self
      self%n = 10
   end function init

end module derived_types_46_m

program derived_types_46
   use derived_types_46_m, only: derived

   type(derived) :: obj
   obj = derived()

   if (obj%n /= 10) error stop

end program derived_types_46
