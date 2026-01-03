module class_52_m
   type, abstract :: AbsType
       integer :: n = 10
   end type AbsType

   type, extends(AbsType) :: MyType
   end type MyType

   interface MyType
      procedure :: init
   end interface MyType

contains

   function init() result(self)
      type(MyType) :: self
      self%n = 44
   end function init

end module class_52_m

program class_52
   use class_52_m

   class(AbsType), allocatable :: obj
   obj = MyType()

   if (obj%n /= 44) error stop
end program class_52
