! MRE: silent wrong answers under --gpu=metal.
!
! A `do concurrent` inside a procedure that a kernel calls is offloaded on
! its own: gpu_offload rewrites it into a host-side kernel launch. When the
! caller's own `do concurrent` is then offloaded, that procedure becomes
! device code -- and a host kernel launch has no lowering in the Metal
! backend, so the statement is silently dropped. The device copy of
! `scaled` is emitted as `inline void scaled(...) { int i; }` and the caller
! reads back whatever the result buffer happened to contain.
!
! Expected: gfortran OK; lfortran (CPU) OK; lfortran --gpu=metal WRONG.
module gpu_metal_247_m
   implicit none
contains

   pure function scaled(v) result(r)
      real, intent(in) :: v(:)
      real :: r(3)
      integer :: i
      do concurrent (i = 1:3)
         r(i) = 2.0*v(i) + real(i)
      end do
   end function

end module

program gpu_metal_247
   use gpu_metal_247_m
   implicit none
   integer, parameter :: n = 5
   real :: a(3, n), b(3, n)
   real :: expected
   integer :: i, j

   do j = 1, n
      do i = 1, 3
         a(i, j) = real(10*j + i)
      end do
   end do
   b = 0.0

   do concurrent (j = 1:n)
      b(:, j) = scaled(a(:, j))
   end do

   do j = 1, n
      do i = 1, 3
         expected = 2.0*real(10*j + i) + real(i)
         if (abs(b(i, j) - expected) > 1.0e-4) error stop "wrong value"
      end do
   end do

   print *, "ok"
end program
