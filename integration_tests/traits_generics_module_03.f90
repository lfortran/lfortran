! LFortran traits extension test; GFortran does not accept this syntax.
! Module nature lookup of generics (F2018 14.2.2): with no module of the
! program of that name, the USE without a nature in the separately compiled
! traits_generics_module_03_plain accesses the intrinsic module, the same one
! that `use, intrinsic ::` accesses here, so the generics constrained by
! either INumeric forward to each other and admit every numeric kind.
module traits_generics_module_03_intrinsic
   use, intrinsic :: generics, only: INumeric
   use traits_generics_module_03_plain, only: plain_total
   implicit none
   private
   public :: twice

contains

   function twice{INumeric :: T}(x) result(r)
      type(T), intent(in) :: x(:)
      type(T)             :: r
      r = plain_total(x) + plain_total{T}(x)
   end function twice
end module traits_generics_module_03_intrinsic

program traits_generics_module_03
   use, intrinsic :: iso_fortran_env, only: int8, int64, real32, real64, real128
   use traits_generics_module_03_plain, only: plain_total
   use traits_generics_module_03_intrinsic, only: twice
   implicit none
   integer(int8)   :: i1(3)
   integer(int64)  :: i8(2)
   real(real32)    :: r4(2)
   real(real128)   :: r16(2)
   complex(real64) :: c8(2)

   i1 = [1_int8, 2_int8, 3_int8]
   i8 = [huge(1_int64) - 1_int64, 1_int64]
   r4 = [0.5_real32, 0.25_real32]
   r16 = [1.0_real128, 2.0_real128**(-100)]
   c8 = [(1.0_real64, 1.0_real64), (2.0_real64, -3.0_real64)]

   if (plain_total(i1) /= 6_int8 .or. kind(plain_total(i1)) /= int8) error stop 1
   if (plain_total(i8) /= huge(1_int64)) error stop 2
   if (plain_total(r4) /= 0.75_real32) error stop 3
   if (plain_total(r16) == 1.0_real128 .or. kind(plain_total(r16)) /= real128) error stop 4
   if (plain_total(c8) /= (3.0_real64, -2.0_real64)) error stop 5
   if (twice(i1) /= 12_int8) error stop 6
   if (twice(c8) /= (6.0_real64, -4.0_real64)) error stop 7
   if (twice{real(real32)}(r4) /= 1.5_real32) error stop 8
   print '(a)', 'traits_generics_module_03: ok'
end program traits_generics_module_03
