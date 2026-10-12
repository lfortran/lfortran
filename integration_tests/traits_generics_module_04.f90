! LFortran traits extension test; GFortran does not accept this syntax.
! The real(real128) member of the intrinsic module's INumeric, through the
! separately compiled clients of traits_generics_module_01 (INTRINSIC uses)
! and traits_generics_module_03 (a USE without a module nature): directly,
! through a forwarding generic and through Adder's binding of ISum. It is a
! test of its own because LLVM 7 and 8 miscompile real(16) operations
! (ci/test_llvm_integration.sh).
program traits_generics_module_04
   use, intrinsic :: iso_fortran_env, only: real128
   use traits_generics_module_01_sums, only: Adder, total
   use traits_generics_module_01_ops, only: twice
   use traits_generics_module_03_plain, only: plain_total
   implicit none
   type(Adder)   :: adder_value
   real(real128) :: r16(2)

   r16 = [1.0_real128, 2.0_real128**(-100)]

   if (total(r16) == 1.0_real128 .or. kind(total(r16)) /= real128) error stop 7
   if (twice(r16) /= 2 * total(r16)) error stop 11
   if (adder_value%sum{real(real128)}(r16) /= total(r16)) error stop 23
   if (plain_total(r16) == 1.0_real128 .or. kind(plain_total(r16)) /= real128) error stop 4
   print '(a)', 'traits_generics_module_04: ok'
end program traits_generics_module_04
