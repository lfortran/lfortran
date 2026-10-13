! LFortran traits extension test; GFortran does not accept this syntax.
! Intrinsic and nonintrinsic module `generics`: the program's own module of
! that name (traits_generics_module_02_generics.f90) is used by a USE without
! a module nature and by `use, non_intrinsic ::`, and its INumeric admits only
! the default integer; `use, intrinsic ::` gets LFortran's intrinsic module,
! whose INumeric admits every numeric kind, although the program's
! generics.mod is on the module search path.
module traits_generics_module_02_plain
   use generics, only: INumeric, origin
   implicit none
   private
   public :: plain_total, plain_origin

contains

   function plain_total{INumeric :: T}(x) result(s)
      type(T), intent(in) :: x(:)
      type(T)             :: s
      integer             :: i
      s = T(0)
      do i = 1, size(x)
         s = s + x(i)
      end do
   end function plain_total

   integer function plain_origin()
      plain_origin = origin()
   end function plain_origin
end module traits_generics_module_02_plain

module traits_generics_module_02_mine
   use, non_intrinsic :: generics, only: Mine => INumeric, origin
   implicit none
   private
   public :: mine_largest, mine_origin

contains

   function mine_largest{Mine :: T}(x) result(m)
      type(T), intent(in) :: x(:)
      type(T)             :: m
      integer             :: i
      m = x(1)
      do i = 2, size(x)
         if (x(i) > m) m = x(i)
      end do
   end function mine_largest

   integer function mine_origin()
      mine_origin = 10 * origin()
   end function mine_origin
end module traits_generics_module_02_mine

module traits_generics_module_02_intrinsic
   use, intrinsic :: generics, only: INumeric
   implicit none
   private
   public :: intrinsic_total

contains

   function intrinsic_total{INumeric :: T}(x) result(s)
      type(T), intent(in) :: x(:)
      type(T)             :: s
      integer             :: i
      s = T(0)
      do i = 1, size(x)
         s = s + x(i)
      end do
   end function intrinsic_total
end module traits_generics_module_02_intrinsic

program traits_generics_module_02
   use, intrinsic :: iso_fortran_env, only: int64, real64
   use traits_generics_module_02_plain, only: plain_total, plain_origin
   use traits_generics_module_02_mine, only: mine_largest, mine_origin
   use traits_generics_module_02_intrinsic, only: intrinsic_total
   implicit none
   integer         :: i4(3)
   integer(int64)  :: i8(2)
   real(real64)    :: r8(2)
   complex(real64) :: c8(2)

   i4 = [3, 9, -2]
   i8 = [5_int64, 6_int64]
   r8 = [0.5_real64, 0.25_real64]
   c8 = [(1.0_real64, 1.0_real64), (2.0_real64, -3.0_real64)]

   if (plain_origin() /= 7) error stop 1
   if (mine_origin() /= 70) error stop 2
   if (plain_total(i4) /= 10) error stop 3
   if (mine_largest(i4) /= 9) error stop 4
   if (intrinsic_total(i4) /= 10) error stop 5
   if (intrinsic_total(i8) /= 11_int64) error stop 6
   if (intrinsic_total(r8) /= 0.75_real64) error stop 7
   if (intrinsic_total(c8) /= (3.0_real64, -2.0_real64)) error stop 8
   print '(a)', 'traits_generics_module_02: ok'
end program traits_generics_module_02
