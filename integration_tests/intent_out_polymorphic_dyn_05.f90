module intent_out_polymorphic_dyn_05_m
   implicit none
   type :: vb
      integer :: h = 4
      integer, allocatable :: x(:)
   end type
   type, extends(vb) :: vbe
      integer :: extra = 9
      integer, allocatable :: y(:)
   end type
   type :: der
      real :: r = 0
      integer, allocatable :: z(:)
   end type
contains
   subroutine s(a)
      class(vb), intent(out) :: a(:)
      integer :: i
      select type (a)
      type is (vbe)
         do i = 1, size(a)
            if (allocated(a(i)%y)) error stop 10
         end do
         allocate (a(1)%y(3))
      end select
   end subroutine
   subroutine s2(a)
      class(vb), intent(out) :: a(:, :)
   end subroutine
   subroutine s_u(a)
      class(*), intent(out) :: a(:)
   end subroutine
end module

program intent_out_polymorphic_dyn_05
   use intent_out_polymorphic_dyn_05_m
   implicit none
   type(vbe) :: y(3), w(2, 2)
   type(der) :: d(2)
   integer :: i

   ! the components an extension adds, of every element
   do i = 1, 3
      y(i)%h = 1
      y(i)%extra = 1
      allocate (y(i)%x(1), y(i)%y(2))
   end do
   call s(y)
   do i = 1, 3
      if (y(i)%h /= 4 .or. y(i)%extra /= 9) error stop 1
      if (allocated(y(i)%x)) error stop 2
   end do
   if (size(y(1)%y) /= 3) error stop 3
   if (allocated(y(2)%y) .or. allocated(y(3)%y)) error stop 4

   ! a section that is not contiguous
   y(2)%extra = 1
   y(3)%extra = 1
   allocate (y(2)%y(1), y(3)%y(1))
   call s(y(1:3:2))
   if (y(1)%extra /= 9 .or. y(3)%extra /= 9) error stop 5
   if (size(y(1)%y) /= 3 .or. allocated(y(3)%y)) error stop 6
   if (y(2)%extra /= 1 .or. .not. allocated(y(2)%y)) error stop 7

   ! rank 2
   w(2, 1)%extra = 0
   allocate (w(1, 2)%y(1))
   call s2(w)
   if (any(w%extra /= 9) .or. allocated(w(1, 2)%y)) error stop 8

   ! unlimited polymorphic
   d(2)%r = 2
   allocate (d(1)%z(2))
   call s_u(d)
   if (d(2)%r /= 0 .or. allocated(d(1)%z)) error stop 9

   print *, "ok"
end program
