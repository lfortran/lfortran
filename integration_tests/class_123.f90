module class_123_mod
   implicit none

   type, abstract :: AbsType
      integer :: val
   contains
      procedure(multintfc), deferred :: mult
      generic :: operator(*) => mult
   end type AbsType

   abstract interface
      function multintfc(a, b) result(r)
         import
         class(AbsType), intent(in) :: a
         class(*),       intent(in) :: b
         class(AbsType), allocatable :: r
      end function multintfc
   end interface

   type, extends(AbsType) :: ConcreteType
   contains
      procedure :: mult => concrete_mult
   end type ConcreteType

contains

   function concrete_mult(a, b) result(r)
      class(ConcreteType), intent(in) :: a
      class(*),            intent(in) :: b
      class(AbsType), allocatable :: r
      allocate(ConcreteType :: r)
      select type (b)
      type is (real(8))
         r%val = int(a%val * b)
      class default
         r%val = 0
      end select
   end function concrete_mult

   subroutine test(d)
      class(AbsType), intent(in) :: d
      class(AbsType), allocatable :: c
      allocate(c, source = (d * 8.d0))
      print *, c%val
      if (c%val /= 40) error stop
   end subroutine test

end module class_123_mod

program class_123
   use class_123_mod
   type(ConcreteType) :: obj
   obj%val = 5
   call test(obj)
end program class_123
