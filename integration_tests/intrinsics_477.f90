program intrinsics_477
   implicit none
   logical,parameter :: T=.true., F=.false.
   integer :: a(3), result

   a(1) = 36
   a(2) = 106
   a(3) = 170

   ! Expected: iany([36,170]) = 36 IOR 170 = 174

   ! iany(array, mask=...) — mask as keyword
   result = iany(a, mask=[T,F,T])
   if (result /= 174) error stop "FAIL: iany(a, mask=[T,F,T]) /= 174"

   ! iany(array, mask) — mask as positional arg (no dim)
   result = iany(a, [T,F,T])
   if (result /= 174) error stop "FAIL: iany(a, [T,F,T]) /= 174"

   ! iany(array, dim, mask) — both dim and mask
   result = iany(a, dim=1, mask=[T,F,T])
   if (result /= 174) error stop "FAIL: iany(a, dim=1, mask=[T,F,T]) /= 174"

   ! iany(array) — no mask, explicit subarray
   result = iany([a(1), a(3)])
   if (result /= 174) error stop "FAIL: iany([a(1),a(3)]) /= 174"

   ! cross-check: masked result must equal explicit subarray result
   if (iany(a,[T,F,T]) /= iany([a(1),a(3)])) error stop "FAIL: masked /= explicit subarray"

   ! negative test: result should not be 1
   if (iany(a,[T,F,T]) == 1) error stop "FAIL: iany(a,[T,F,T]) should not be 1"

end program intrinsics_477
