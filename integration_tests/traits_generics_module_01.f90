! LFortran traits extension test; GFortran does not accept this syntax.
! `use, intrinsic :: generics, only: INumeric` needs no provider of the
! program: the runtime installs the module. The predefined INumeric admits
! every integer, real and complex kind; generic calls through it are checked
! for representative and nondefault kinds, directly, through a forwarding
! generic of another module and through Adder's binding of ISum.
program traits_generics_module_01
   use, intrinsic :: iso_fortran_env, only: int8, int16, int64, real32, &
      real64, real128
   use traits_generics_module_01_sums, only: Adder, total
   use traits_generics_module_01_ops, only: twice
   implicit none
   type(Adder)     :: adder_value
   integer(int8)   :: i1(3)
   integer(int16)  :: i2(2)
   integer         :: i4(2)
   integer(int64)  :: i8(2)
   real(real32)    :: r4(2)
   real(real64)    :: r8(3)
   real(real128)   :: r16(2)
   complex(real32) :: c4(2)
   complex(real64) :: c8(2)

   i1 = [1_int8, 2_int8, 3_int8]
   i2 = [1000_int16, -1_int16]
   i4 = [40, 2]
   i8 = [huge(1_int64) - 1_int64, 1_int64]
   r4 = [0.5_real32, 0.25_real32]
   r8 = [1.5_real64, 2.0_real64, -0.25_real64]
   r16 = [1.0_real128, 2.0_real128**(-100)]
   c4 = [(1.0_real32, 2.0_real32), (3.0_real32, -1.0_real32)]
   c8 = [(0.5_real64, 0.0_real64), (0.0_real64, 1.5_real64)]

   if (total(i1) /= 6_int8 .or. kind(total(i1)) /= int8) error stop 1
   if (total(i2) /= 999_int16 .or. kind(total(i2)) /= int16) error stop 2
   if (total(i4) /= 42) error stop 3
   if (total(i8) /= huge(1_int64) .or. kind(total(i8)) /= int64) error stop 4
   if (total(r4) /= 0.75_real32 .or. kind(total(r4)) /= real32) error stop 5
   if (total(r8) /= 3.25_real64) error stop 6
   if (total(r16) == 1.0_real128 .or. kind(total(r16)) /= real128) error stop 7
   if (total(c4) /= (4.0_real32, 1.0_real32) .or. kind(total(c4)) /= real32) error stop 8
   if (total{complex(real64)}(c8) /= (0.5_real64, 1.5_real64)) error stop 9

   if (twice(i1) /= 12_int8 .or. kind(twice(i1)) /= int8) error stop 10
   if (twice(r16) /= 2 * total(r16)) error stop 11
   if (twice(c8) /= (1.0_real64, 3.0_real64)) error stop 12
   if (twice{real(real32)}(r4) /= 1.5_real32) error stop 13

   if (adder_value%sum(i2) /= 999_int16) error stop 20
   if (adder_value%sum(r8(1:2)) /= 3.5_real64) error stop 21
   if (adder_value%sum(c4) /= (4.0_real32, 1.0_real32)) error stop 22
   if (adder_value%sum{real(real128)}(r16) /= total(r16)) error stop 23
   print '(a)', 'traits_generics_module_01: ok'
end program traits_generics_module_01
