! An array section passed as an actual argument to a procedure called from
! an offloaded `do concurrent` reaches that procedure as a base pointer plus
! an element count.  Neither carries a stride, so a strided section used to
! be read back as the contiguous run it starts on: silent wrong answers.
!
! A strided section is now gathered into a contiguous per-thread temporary
! before the call.  A unit-stride section and a whole array still take the
! cheap base-pointer path, and are checked here as fences.
module gpu_metal_248_m
   implicit none

   type :: box_t
      real :: c(9)
   end type

contains

   pure function s3(v) result(r)
      real, intent(in) :: v(:)
      real :: r
      r = 100.0*v(1) + 10.0*v(2) + v(3)
   end function

end module

program gpu_metal_248
   use gpu_metal_248_m
   implicit none
   integer, parameter :: n = 4
   real :: a(20)
   real :: strided(n), negstep(n), structc(n), localcopy(n)
   real :: assoc(n), unitsec(n), whole(n), twodim(n)
   real :: fixed(3), plane(5, n)
   type(box_t) :: box
   real :: expected
   integer :: i, j

   do i = 1, 20
      a(i) = real(i)
   end do
   do i = 1, 9
      box%c(i) = real(10*i)
   end do
   do i = 1, 3
      fixed(i) = real(i)
   end do
   do j = 1, n
      do i = 1, 5
         plane(i, j) = real(10*j + i)
      end do
   end do

   strided = 0.0
   negstep = 0.0
   structc = 0.0
   localcopy = 0.0
   assoc = 0.0
   unitsec = 0.0
   whole = 0.0
   twodim = 0.0

   do concurrent (j = 1:n)
      ! stride 2: a(j), a(j+2), a(j+4)
      strided(j) = s3(a(j:j + 4:2))
      ! stride -2: a(j+4), a(j+2), a(j)
      negstep(j) = s3(a(j + 4:j: - 2))
      ! a strided section of a derived-type component
      structc(j) = s3(box%c(j:j + 4:2))
      ! a strided section of the second dimension of a rank-2 array
      twodim(j) = s3(plane(1:5:2, j))
      ! fence: unit stride
      unitsec(j) = s3(a(j:j + 2))
      ! fence: whole array
      whole(j) = s3(fixed)
   end do

   do concurrent (j = 1:n)
      block
         real :: tmp(3)
         ! a strided section copied into a local first
         tmp = a(j:j + 4:2)
         localcopy(j) = s3(tmp)
      end block
   end do

   do concurrent (j = 1:n)
      associate (q => j + 1)
         ! a strided section whose bounds come from an associate name
         assoc(j) = s3(a(q:q + 4:2))
      end associate
   end do

   do j = 1, n
      expected = 100.0*real(j) + 10.0*real(j + 2) + real(j + 4)
      if (abs(strided(j) - expected) > 1.0e-4) error stop "strided"
      if (abs(localcopy(j) - expected) > 1.0e-4) error stop "local copy"

      expected = 100.0*real(j + 4) + 10.0*real(j + 2) + real(j)
      if (abs(negstep(j) - expected) > 1.0e-4) error stop "negative step"

      expected = 100.0*real(10*j) + 10.0*real(10*(j + 2)) + real(10*(j + 4))
      if (abs(structc(j) - expected) > 1.0e-4) error stop "struct component"

      expected = 100.0*real(10*j + 1) + 10.0*real(10*j + 3) + real(10*j + 5)
      if (abs(twodim(j) - expected) > 1.0e-4) error stop "rank-2 section"

      expected = 100.0*real(j + 1) + 10.0*real(j + 3) + real(j + 5)
      if (abs(assoc(j) - expected) > 1.0e-4) error stop "associate"

      expected = 100.0*real(j) + 10.0*real(j + 1) + real(j + 2)
      if (abs(unitsec(j) - expected) > 1.0e-4) error stop "unit stride"

      if (abs(whole(j) - 123.0) > 1.0e-4) error stop "whole array"
   end do

   print *, "ok"
end program
