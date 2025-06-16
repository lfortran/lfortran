module class_53_m
   type, abstract :: AbsType
       integer :: m = 0
   end type AbsType

   type, extends(AbsType) :: MyType
   end type MyType

   interface MyType
      procedure :: init
   end interface MyType

   type :: Wrapper
      class(AbsType), allocatable :: obj
   contains
      procedure :: client
   end type Wrapper

contains

   function init() result(self)
      type(MyType) :: self

      self%m = 44
   end function init

   subroutine client(self)
      class(Wrapper), intent(inout) :: self
      self%obj = MyType()

      if (self%obj%m /= 44) error stop
   end subroutine client

end module class_53_m

program class_53
    use class_53_m
    type(Wrapper) :: w

    call w%client()
end program
