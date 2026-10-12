module basic_interfaces

   implicit none
   private

   public :: IReducible, IPrintable

   abstract interface :: IReducible
      initial(n)
         integer, intent(in) :: n
      end initial
      elemental function operator(*)(lhs,rhs) result(res)
         type(itself), intent(in) :: lhs, rhs
         type(itself)             :: res
      end function operator(*)
      elemental function operator(*)(lhs,rhs) result(res)
         type(itself), intent(in) :: lhs
         integer,      intent(in) :: rhs
         type(itself)             :: res
      end function operator(*)
   end interface IReducible
   
   abstract interface :: IPrintable
      subroutine output()
      end subroutine output
   end interface IPrintable

end module basic_interfaces

module my_type

   use basic_interfaces, only: IReducible, IPrintable

   implicit none
   private
   
   type, public, sealed :: MyType
      private
      integer :: n
   end type MyType

   implements (IReducible + IPrintable) :: MyType
      initial :: init
      procedure, static :: operator(*) => multiply, multiply_by_int
      procedure, pass   :: output
   end implements MyType

contains
   
   function init(n) result(res)
      integer, intent(in) :: n
      type(MyType)        :: res
      res%n = n
   end function init
   
   elemental function multiply(lhs,rhs) result(res)
      type(MyType), intent(in) :: lhs, rhs
      type(MyType)             :: res
      res%n = lhs%n * rhs%n
   end function multiply

   elemental function multiply_by_int(lhs,rhs) result(res)
      type(MyType), intent(in) :: lhs
      integer,      intent(in) :: rhs
      type(MyType)             :: res
      res%n = lhs%n * rhs
   end function multiply_by_int
   
   subroutine output(self)
      type(MyType), intent(in) :: self
      write(*,'(a,i8)') "I am: ", self%n
   end subroutine output

end module my_type

module real_type

   use basic_interfaces, only: IReducible, IPrintable

   implicit none
   private
   
   implements (IReducible + IPrintable) :: real
      procedure, pass :: output
   end implements real

contains

   subroutine output(self)
      real, intent(in) :: self
      write(*,'(a,f8.1)') "I am: ", self
   end subroutine output

end module real_type

program main

   use basic_interfaces, only: IReducible, IPrintable
   use my_type,          only: MyType
   use real_type

   implicit none

   ! declare and initialize arrays with four elements
   at := [(MyType(i),i:=1,4)]  ! Use user-defined initializer for MyType
   ar := [(real(i),i:=1,4)]    ! Use built-in initializer/cast for real type
   
   call products(at,ar)
   
contains

   subroutine products{IReducible + IPrintable :: T, &
                       IReducible + IPrintable :: R}(at,ar)
      type(T), intent(in) :: at(4)
      type(R), intent(in) :: ar(4)
      st := prod( (at * at) * [4,3,2,1] )   ! Reduce arrays to scalars after
      sr := prod( (ar * ar) * [4,3,2,1] )   ! elementwise multiplications
      call st%output()                      ! Print the results
      call sr%output()                      ! (13824 in both cases)
   end subroutine products

   function prod{IReducible :: T}(arr) result(res)
      type(T), intent(in) :: arr(:)
      type(T)             :: res
      res = T(1)                            ! Use initializer for cast
      do i := 1, size(arr)
         res = res * arr(i)                 ! Use operator(*) for reduction
      end do
   end function prod
   
end program main
