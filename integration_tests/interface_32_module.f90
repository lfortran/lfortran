module interface_32_mymod

   type :: MyType
      integer :: val = 0
   end type MyType

   interface MyType
      procedure :: constructor
   end interface MyType

contains

   function constructor() result(self)
      type(MyType) :: self
      self%val = 42
   end function constructor

end module interface_32_mymod

module interface_32_wrappermod

   use interface_32_mymod, only: MyType

   type :: WrapperType
      class(MyType), allocatable :: obj
   end type WrapperType

   interface WrapperType
      procedure :: constructor
   end interface WrapperType

contains

   function constructor(obj) result(self)
      class(MyType), intent(in) :: obj
      type(WrapperType)         :: self
      allocate(self%obj, source=obj)
   end function constructor

end module interface_32_wrappermod
