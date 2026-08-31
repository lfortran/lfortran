program gpu_metal_259
   ! An element of an array of derived type reached through a component of
   ! a kernel argument -- `f%ops_(xd)` -- is copied into a temporary on the
   ! host before the launch, so the kernel sees an ordinary derived-type
   ! argument whose own components carry their extents.
   implicit none

   integer, parameter :: xd = 1, yd = 2

   type :: op_t
      real, allocatable :: u_(:,:)
      real, allocatable :: v_(:)
      integer :: k_
   end type

   type :: field_t
      type(op_t), allocatable :: ops_(:)
   end type

   type(field_t) :: f
   real :: a(4), b(4), c(4), d(4), e(4), g(4)
   real :: plain(4)
   integer :: pick(4)
   integer :: j, n

   allocate(f%ops_(2))
   allocate(f%ops_(xd)%u_(2,3), f%ops_(xd)%v_(3))
   allocate(f%ops_(yd)%u_(2,3), f%ops_(yd)%v_(3))

   f%ops_(xd)%u_ = reshape([1., 2., 3., 4., 5., 6.], [2,3])
   f%ops_(xd)%v_ = [10., 20., 30.]
   f%ops_(xd)%k_ = 7
   f%ops_(yd)%u_ = reshape([-1., -2., -3., -4., -5., -6.], [2,3])
   f%ops_(yd)%v_ = [-10., -20., -30.]
   f%ops_(yd)%k_ = -7

   plain = [100., 200., 300., 400.]
   pick = [xd, yd, xd, yd]
   n = 4
   a = 0.; b = 0.; c = 0.; d = 0.; e = 0.; g = 0.

   ! Loop-invariant subscript, read only: every one of these is gathered.
   do concurrent (j = 1:n)
      ! a scalar component of the selected element
      a(j) = real(f%ops_(xd)%k_) + real(j)
      ! a rank-2 component read elementwise
      b(j) = f%ops_(xd)%u_(2,3) * real(j)
      ! a rank-2 component read whole
      c(j) = sum(f%ops_(xd)%u_) + real(j)
      ! a rank-1 component read elementwise and whole
      d(j) = f%ops_(xd)%v_(2) * real(j) + sum(f%ops_(xd)%v_)
      ! a second element of the same array of structs
      e(j) = real(f%ops_(yd)%k_) - real(j)
      ! a plain array argument as a fence
      g(j) = plain(j) + real(f%ops_(yd)%k_)
   end do

   print *, a
   print *, b
   print *, c
   print *, d
   print *, e
   print *, g

   do j = 1, n
      if (abs(a(j) - (7.0 + real(j))) > 1e-5) error stop "a"
      if (abs(b(j) - 6.0*real(j)) > 1e-5) error stop "b"
      if (abs(c(j) - (21.0 + real(j))) > 1e-5) error stop "c"
      if (abs(d(j) - (20.0*real(j) + 60.0)) > 1e-5) error stop "d"
      if (abs(e(j) - (-7.0 - real(j))) > 1e-5) error stop "e"
      if (abs(g(j) - (plain(j) - 7.0)) > 1e-5) error stop "g"
   end do

   ! A loop-variant subscript cannot be hoisted, so it is not gathered.
   ! The loop is declined for offload and runs correctly on the host.
   a = 0.
   do concurrent (j = 1:n)
      a(j) = real(f%ops_(pick(j))%k_) * real(j)
   end do
   print *, a
   do j = 1, n
      if (abs(a(j) - real(f%ops_(pick(j))%k_) * real(j)) > 1e-5) then
         error stop "variant"
      end if
   end do

end program
