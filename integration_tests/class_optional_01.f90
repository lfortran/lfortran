module class_optional_01_mod
   implicit none
   type :: MyType
      integer, allocatable :: s
   end type MyType
   interface MyType
      procedure :: my_constructor
   end interface MyType
contains
   function my_constructor(obj) result(self)
      class(MyType), optional, intent(in) :: obj
      type(MyType) :: self
      if (present(obj)) then
         self%s = 10
      else
         self%s = 5
      end if
   end function my_constructor
end module class_optional_01_mod

program class_optional_01
   use class_optional_01_mod
   implicit none
   type(MyType) :: a

   ! Call constructor with no arguments (optional class arg absent).
   ! Previously caused "pointer being freed was not allocated" crash
   ! because the class wrapper for the absent optional argument had
   ! its inner struct pointer left uninitialized.
   a = MyType()
   if (.not. allocated(a%s)) error stop "a%s should be allocated"
   if (a%s /= 5) error stop "a%s should be 5"

   print *, "PASS"
end program class_optional_01
