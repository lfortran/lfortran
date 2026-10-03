module interface_32_clientmod

   use interface_32_mymod
   use interface_32_wrappermod, only: WrapperType

contains

   function init() result(wrap)
      type(WrapperType) :: wrap
      type(MyType)      :: obj
      obj = MyType()
      wrap = WrapperType(obj)
   end function init

end module interface_32_clientmod

program interface_32
   use interface_32_mymod
   use interface_32_wrappermod
   use interface_32_clientmod
   implicit none

   type(WrapperType) :: w

   w = init()
   if (w%obj%val /= 42) error stop
   print *, "PASS"
end program interface_32

