! The separately compiled generic type SimpleSum and the type BasicSum that
! traits_generic_type_extends_05.f90 gives it as type argument.
module traits_generic_type_extends_05_interfaces

   use, intrinsic :: iso_fortran_env, only: real64

   implicit none
   private
   
   public :: INumeric, ISum, IAverager
   
   abstract interface :: INumeric
      integer | real(real64)
   end interface INumeric

   abstract interface :: ISum
      function sum{INumeric :: T}(self,x) result(s)
         type(itself), intent(in), pass :: self
         type(T),      intent(in)       :: x(:)
         type(T)                        :: s
      end function sum
   end interface ISum

   abstract interface :: IAverager
      function average{INumeric :: T}(self,x) result(a)
         type(itself), intent(in), pass :: self
         type(T),      intent(in)       :: x(:)
         type(T)                        :: a
      end function average
   end interface IAverager

end module traits_generic_type_extends_05_interfaces


module traits_generic_type_extends_05_basic

   use traits_generic_type_extends_05_interfaces, only: ISum, INumeric

   implicit none
   private

   public :: BasicSum

   type, implements(ISum) :: BasicSum
   contains
      procedure :: sum
   end type BasicSum

contains

   function sum{INumeric :: T}(self,x) result(s)
      class(BasicSum), intent(in) :: self
      type(T),         intent(in) :: x(:)
      type(T)                     :: s
      s = T(0)
      do i := 1, size(x)
         s = s + x(i)
      end do
   end function sum

end module traits_generic_type_extends_05_basic


module traits_generic_type_extends_05_provider

   use traits_generic_type_extends_05_interfaces, only: ISum, INumeric

   implicit none
   private

   public :: SimpleSum
   
   type, implements(ISum) :: SimpleSum{ISum :: U}
      type(U), allocatable :: basic
   contains
      initial   :: init
      procedure :: sum
   end type SimpleSum
   
contains

   function init(basic) result(res)
      type(U), intent(in) :: basic
      type(SimpleSum{U})  :: res
      res%basic = basic
   end function init
   
   function sum{INumeric :: T}(self,x) result(s)
      class(SimpleSum{U}), intent(in) :: self
      type(T),             intent(in) :: x(:)
      type(T)                         :: s
      s = self%basic%sum(x)
   end function sum

end module traits_generic_type_extends_05_provider

