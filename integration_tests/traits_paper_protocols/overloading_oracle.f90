! Standard-Fortran oracle for the paper's Code/Fortran/overloading.f90
! (companion revision 5163a026, sha256
! 9697a242ce03a441460b340660b52ade97357fdf2bb94389286ae977bd070e92; the
! acceptance copy adds one "use real_type" to the program). IReducible and
! IPrintable only constrain generic procedures, which the paper instantiates
! at compile time, so the oracle writes out the instantiations the program
! uses: products for T = MyType and R = real, and prod for MyType and for
! real, with unchanged bodies. MyType's initializer becomes a generic
! interface named like the type, and its static elemental operator(*)
! bindings become a generic operator interface, which standard Fortran
! imports explicitly. real keeps its intrinsic initializer and elemental
! multiplication (also by integer); only output is user-provided. The
! trait-only module basic_interfaces has no standard counterpart. :=
! declarations and inferred DO indices become explicit declarations.
module overloading_oracle_my_type

   implicit none
   private

   public :: MyType, operator(*)

   type :: MyType
      private
      integer :: n
   contains
      procedure, pass :: output
   end type MyType

   ! initial :: init
   interface MyType
      module procedure init
   end interface MyType

   ! procedure, static :: operator(*) => multiply, multiply_by_int
   interface operator(*)
      module procedure multiply, multiply_by_int
   end interface operator(*)

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
      class(MyType), intent(in) :: self
      write(*,'(a,i8)') "I am: ", self%n
   end subroutine output

end module overloading_oracle_my_type

module overloading_oracle_real_type

   implicit none
   private

   public :: output

contains

   subroutine output(self)
      real, intent(in) :: self
      write(*,'(a,f8.1)') "I am: ", self
   end subroutine output

end module overloading_oracle_real_type

program main

   use overloading_oracle_my_type,   only: MyType, operator(*)
   use overloading_oracle_real_type, only: output

   implicit none

   type(MyType) :: at(4)
   real         :: ar(4)
   integer      :: i

   ! declare and initialize arrays with four elements
   at = [(MyType(i),i=1,4)]  ! Use user-defined initializer for MyType
   ar = [(real(i),i=1,4)]    ! Use built-in initializer/cast for real type

   call products(at,ar)

contains

   ! products{IReducible + IPrintable :: T, IReducible + IPrintable :: R}
   ! instantiated for T = MyType and R = real
   subroutine products(at,ar)
      type(MyType), intent(in) :: at(4)
      real,         intent(in) :: ar(4)
      type(MyType)             :: st
      real                     :: sr
      st = prod_mytype( (at * at) * [4,3,2,1] )   ! Reduce arrays to scalars after
      sr = prod_real( (ar * ar) * [4,3,2,1] )     ! elementwise multiplications
      call st%output()                            ! Print the results
      call output(sr)                             ! (13824 in both cases)
   end subroutine products

   ! prod{IReducible :: T} instantiated for T = MyType
   function prod_mytype(arr) result(res)
      type(MyType), intent(in) :: arr(:)
      type(MyType)             :: res
      integer                  :: i
      res = MyType(1)                       ! Use initializer for cast
      do i = 1, size(arr)
         res = res * arr(i)                 ! Use operator(*) for reduction
      end do
   end function prod_mytype

   ! prod{IReducible :: T} instantiated for T = real
   function prod_real(arr) result(res)
      real, intent(in) :: arr(:)
      real             :: res
      integer          :: i
      res = real(1)                         ! Use initializer for cast
      do i = 1, size(arr)
         res = res * arr(i)                 ! Use operator(*) for reduction
      end do
   end function prod_real

end program main
