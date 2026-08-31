module gpu_metal_262_m
   ! A type-bound call names a binding, not the procedure itself. The
   ! offload pass has to step through the binding to read the dummy
   ! intents; otherwise every actual argument of every type-bound call
   ! reads as written and the struct-element gather is declined.
   implicit none

   type :: op_t
      real, allocatable :: u_(:,:)
      real, allocatable :: v_(:)
      integer :: k_
   end type

   type :: field_t
      type(op_t), allocatable :: ops_(:)
   end type

   type :: scaler_t
      real :: w_
   contains
      procedure :: weight
      procedure :: bump
   end type

contains

   pure function weight(self, x) result(r)
      class(scaler_t), intent(in) :: self
      real, intent(in) :: x
      real :: r
      r = self%w_ * x
   end function

   pure subroutine bump(self, x)
      class(scaler_t), intent(in) :: self
      real, intent(inout) :: x
      x = x + self%w_
   end subroutine

   ! A `class(...)` dummy whose subscripted struct-array element is read
   ! only, through a type-bound function all of whose dummies are
   ! `intent(in)`. Offloaded.
   subroutine gather_class(self, s, a, n)
      class(field_t), intent(in) :: self
      type(scaler_t), intent(in) :: s
      real, intent(out) :: a(:)
      integer, intent(in) :: n
      integer :: j
      do concurrent (j = 1:n)
         a(j) = s%weight(self%ops_(1)%v_(j)) + self%ops_(1)%u_(1,1)*real(j)
      end do
   end subroutine

   ! The same shape with a `type(...)` dummy, as a fence: the gather must
   ! not depend on the root being polymorphic.
   subroutine gather_type(self, s, a, n)
      type(field_t), intent(in) :: self
      type(scaler_t), intent(in) :: s
      real, intent(out) :: a(:)
      integer, intent(in) :: n
      integer :: j
      do concurrent (j = 1:n)
         a(j) = s%weight(self%ops_(2)%v_(j)) - real(self%ops_(2)%k_)
      end do
   end subroutine

   ! A loop-variant subscript cannot be hoisted, so the element is not
   ! gathered and the loop is declined. It still has to compute the
   ! right answer on the host.
   subroutine variant_subscript(self, a, n, pick)
      class(field_t), intent(in) :: self
      real, intent(out) :: a(:)
      integer, intent(in) :: n
      integer, intent(in) :: pick(:)
      integer :: j
      do concurrent (j = 1:n)
         a(j) = real(self%ops_(pick(j))%k_) * real(j)
      end do
   end subroutine

   ! The loop writes through the very element that would be gathered --
   ! a type-bound subroutine with an `intent(inout)` dummy -- so a copy
   ! made before the launch would be stale. Declined.
   subroutine written_chain(self, s, a, n)
      type(field_t), intent(inout) :: self
      type(scaler_t), intent(in) :: s
      real, intent(out) :: a(:)
      integer, intent(in) :: n
      integer :: j
      do concurrent (j = 1:n)
         call s%bump(self%ops_(1)%v_(j))
         a(j) = real(j)
      end do
   end subroutine

end module

program gpu_metal_262
   use gpu_metal_262_m
   implicit none

   type(field_t) :: f
   type(scaler_t) :: s
   real :: a(4)
   integer :: pick(4)
   integer :: j
   integer, parameter :: n = 4

   allocate(f%ops_(2))
   allocate(f%ops_(1)%u_(2,3), f%ops_(1)%v_(4))
   allocate(f%ops_(2)%u_(2,3), f%ops_(2)%v_(4))

   f%ops_(1)%u_ = reshape([1., 2., 3., 4., 5., 6.], [2,3])
   f%ops_(1)%v_ = [10., 20., 30., 40.]
   f%ops_(1)%k_ = 7
   f%ops_(2)%u_ = reshape([-1., -2., -3., -4., -5., -6.], [2,3])
   f%ops_(2)%v_ = [-10., -20., -30., -40.]
   f%ops_(2)%k_ = -7

   s%w_ = 3.

   pick = [1, 2, 1, 2]

   a = 0.
   call gather_class(f, s, a, n)
   print *, a
   do j = 1, n
      if (abs(a(j) - (3.*f%ops_(1)%v_(j) + 1.*real(j))) > 1e-4) error stop "a"
   end do

   a = 0.
   call gather_type(f, s, a, n)
   print *, a
   do j = 1, n
      if (abs(a(j) - (3.*f%ops_(2)%v_(j) + 7.)) > 1e-4) error stop "b"
   end do

   a = 0.
   call variant_subscript(f, a, n, pick)
   print *, a
   do j = 1, n
      if (abs(a(j) - real(f%ops_(pick(j))%k_)*real(j)) > 1e-4) error stop "c"
   end do

   a = 0.
   call written_chain(f, s, a, n)
   print *, f%ops_(1)%v_
   do j = 1, n
      if (abs(f%ops_(1)%v_(j) - (10.*real(j) + 3.)) > 1e-4) error stop "d"
      if (abs(a(j) - real(j)) > 1e-4) error stop "e"
   end do

end program
