program intrinsics_477
   implicit none
   logical, parameter :: T = .true., F = .false.
   integer :: a(3), result
   integer :: a2d(2, 3), res1d(3)
   logical :: scalar_mask

   a(1) = 36
   a(2) = 106
   a(3) = 170

   ! --- iany tests ---

   ! iany(array, mask=...) — mask as keyword
   result = iany(a, mask=[T, F, T])
   if (result /= 174) error stop "FAIL: iany(a, mask=[T,F,T]) /= 174"

   ! iany(array, mask) — mask as positional arg (no dim)
   result = iany(a, [T, F, T])
   if (result /= 174) error stop "FAIL: iany(a, [T,F,T]) /= 174"

   ! iany(array, dim=1) — no mask, 1-D input => scalar output
   result = iany(a, dim=1)
   if (result /= 238) error stop "FAIL: iany(a, dim=1) /= 238"

   ! iany(array, dim, mask) — both dim and mask on 1-D array
   result = iany(a, dim=1, mask=[T, F, T])
   if (result /= 174) error stop "FAIL: iany(a, dim=1, mask=[T,F,T]) /= 174"

   ! iany(array) — no mask, explicit subarray
   result = iany([a(1), a(3)])
   if (result /= 174) error stop "FAIL: iany([a(1),a(3)]) /= 174"

   ! cross-check: masked result must equal explicit subarray result
   if (iany(a, [T, F, T]) /= iany([a(1), a(3)])) error stop "FAIL: masked /= explicit subarray"

   ! scalar mask — iany(a, mask=.true.) is legal per F2018 §3.36
   scalar_mask = .true.
   result = iany(a, mask=scalar_mask)
   if (result /= 238) error stop "FAIL: iany(a, mask=.true.) /= 238"

   ! 2-D array: iany(a2d, dim=1) reduces along first dim => result is rank-1
   a2d(1, 1) = 36;  a2d(2, 1) = 106
   a2d(1, 2) = 170; a2d(2, 2) = 0
   a2d(1, 3) = 15;  a2d(2, 3) = 240
   res1d = iany(a2d, dim=1)
   if (res1d(1) /= ior(36, 106))  error stop "FAIL: iany(a2d, dim=1)(1)"
   if (res1d(2) /= ior(170, 0))   error stop "FAIL: iany(a2d, dim=1)(2)"
   if (res1d(3) /= ior(15, 240))  error stop "FAIL: iany(a2d, dim=1)(3)"

   ! 2-D array: iany(a2d, dim=1, mask=...) reduces along first dim with mask
   res1d = iany(a2d, dim=1, mask=reshape([T, F, T, T, F, T], shape(a2d)))
   if (res1d(1) /= 36)   error stop "FAIL: iany(a2d,dim=1,mask)(1)"
   if (res1d(2) /= 170)  error stop "FAIL: iany(a2d,dim=1,mask)(2)"
   if (res1d(3) /= 240)  error stop "FAIL: iany(a2d,dim=1,mask)(3)"

   ! --- iall tests (mask path was missing coverage) ---

   ! iall(array) — all: 36 AND 170 AND 106 = 32
   result = iall(a)
   if (result /= iand(iand(36, 106), 170)) error stop "FAIL: iall(a)"

   ! iall(array, mask=...) — mask as keyword
   result = iall(a, mask=[T, F, T])
   if (result /= iand(36, 170)) error stop "FAIL: iall(a, mask=[T,F,T])"

   ! iall(array, mask) — mask as positional arg
   result = iall(a, [T, F, T])
   if (result /= iand(36, 170)) error stop "FAIL: iall(a, [T,F,T])"

   ! iall(array, dim=1) — 1-D => scalar output
   result = iall(a, dim=1)
   if (result /= iand(iand(36, 106), 170)) error stop "FAIL: iall(a, dim=1)"

   ! iall with scalar mask
   scalar_mask = .true.
   result = iall(a, mask=scalar_mask)
   if (result /= iand(iand(36, 106), 170)) error stop "FAIL: iall(a, mask=.true.)"

end program intrinsics_477
