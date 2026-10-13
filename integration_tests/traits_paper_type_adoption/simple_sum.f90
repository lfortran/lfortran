module interfaces

   use, intrinsic :: iso_fortran_env, only: real64

   implicit none
   private
   
   public :: INumeric, ISum, IAverager
   
   abstract interface :: INumeric
      integer | real(real64)
   end interface INumeric

   abstract interface :: ISum
      function sum{INumeric :: T}(x) result(s)
         type(T), intent(in) :: x(:)
         type(T)             :: s
      end function sum
   end interface ISum

   abstract interface :: IAverager
      function average{INumeric :: T}(x) result(a)
         type(T), intent(in) :: x(:)
         type(T)             :: a
      end function average
   end interface IAverager

end module interfaces

module simple_library

   use interfaces, only: ISum, INumeric

   implicit none
   private

   public :: SimpleSum
   
   type, sealed, implements(ISum) :: SimpleSum
   contains
      procedure, nopass :: sum
   end type SimpleSum

contains
   
   function sum{INumeric :: T}(x) result(s)
      type(T), intent(in) :: x(:)
      type(T)             :: s
      s = T(0)
      do i := 1, size(x)
         s = s + x(i)
      end do
   end function sum

end module simple_library
