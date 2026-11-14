module intent_out_polymorphic_dyn_04_m
   implicit none
   type :: vb
      integer :: h = 4
   end type
   type, extends(vb) :: vbe
      integer :: extra = 9
      real, allocatable :: x(:)
      character(len=5) :: name = "hello"
   end type
   type, extends(vbe) :: vbe2
      type(vb) :: part
      logical :: flag = .true.
   end type
   type :: der
      real :: r = 0
   end type
contains
   subroutine s_sc(a)
      class(vb), intent(out) :: a
   end subroutine
   subroutine s_k(a)
      class(vb), intent(out) :: a
      a%h = 3
   end subroutine
   subroutine s_u(a)
      class(*), intent(out) :: a
   end subroutine
end module

program intent_out_polymorphic_dyn_04
   use intent_out_polymorphic_dyn_04_m
   implicit none
   type(vbe) :: y
   type(vbe2) :: z
   type(der) :: d
   class(vb), allocatable :: p

   ! the components an extension adds get their default values
   y%h = 1
   y%extra = 1
   y%name = "xxxxx"
   call s_sc(y)
   if (y%h /= 4 .or. y%extra /= 9 .or. y%name /= "hello") error stop 1

   ! and its allocatable components are deallocated
   allocate (y%x(3))
   y%extra = 2
   call s_k(y)
   if (y%h /= 3 .or. y%extra /= 9) error stop 2
   if (allocated(y%x)) error stop 3

   ! two levels of extension, with a component of derived type
   z%h = 0
   z%extra = 0
   z%part%h = 0
   z%flag = .false.
   allocate (z%x(1))
   call s_sc(z)
   if (z%h /= 4 .or. z%extra /= 9 .or. z%part%h /= 4) error stop 4
   if (.not. z%flag .or. z%name /= "hello" .or. allocated(z%x)) error stop 5

   ! an allocatable class entity
   allocate (vbe :: p)
   select type (p)
   type is (vbe)
      p%extra = 7
      allocate (p%x(2))
   end select
   call s_sc(p)
   select type (p)
   type is (vbe)
      if (p%extra /= 9 .or. allocated(p%x)) error stop 6
   class default
      error stop 7
   end select

   ! unlimited polymorphic
   d%r = 2
   call s_u(d)
   if (d%r /= 0) error stop 8

   print *, "ok"
end program
