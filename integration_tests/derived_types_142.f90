module mytypemod_derived_types_142

   type :: MyType
      real(8), pointer :: ptr(:)
   end type MyType

   interface MyType
      procedure :: constructor
   end interface MyType
   
contains

   function constructor() result(self)
      type(MyType) :: self   
   end function constructor

end module mytypemod_derived_types_142


module wrappermod_derived_types_142
   
   use mytypemod_derived_types_142, only: MyType
   
   type :: WrapperType
      private
      class(MyType), allocatable :: obj
   end type WrapperType
   
   interface WrapperType
      procedure :: constructor
   end interface WrapperType

contains
   
   function constructor(obj) result(self)
      class(MyType), intent(in) :: obj
      type(WrapperType)         :: self
      self%obj = obj
   end function constructor

end module wrappermod_derived_types_142


program derived_types_142

   use wrappermod_derived_types_142, only: WrapperType
   use mytypemod_derived_types_142,  only: MyType
   
   call wrapping()
   
contains
   
   subroutine wrapping()
      class(WrapperType), allocatable :: wrap
      wrap = WrapperType(MyType())
   end subroutine wrapping

end program