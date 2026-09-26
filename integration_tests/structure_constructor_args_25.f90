! The value of an intrinsic assignment is evaluated before the target is
! defined (F2018 10.2.1.3). A structure constructor whose arguments read the
! target, directly or through a pointer, an associate name, or a function,
! must therefore see the target's old components, not the ones an earlier
! argument has already given a new value.
module structure_constructor_args_25_mod
   implicit none

   type :: a_t
      integer :: x = 0
      integer :: y = 0
   end type a_t

   type :: o_t
      type(a_t) :: a
      type(a_t) :: b
      integer :: k = 0
   end type o_t

   type :: n_t
      type(a_t), pointer :: p => null()
   end type n_t

   type :: s_t
      character(len=:), allocatable :: s
      integer, allocatable :: v(:)
      integer :: n = 0
   end type s_t

   type(a_t) :: g

contains

   integer function get_gx()
      get_gx = g%x
   end function get_gx

   subroutine swap(t)
      type(a_t), intent(inout) :: t
      t = a_t(t%y, t%x)
   end subroutine swap

   subroutine host()
      type(a_t) :: h
      h = a_t(1, 2)
      h = a_t(x=10, y=read_h())
      if (h%x /= 10 .or. h%y /= 1) error stop 30
   contains
      integer function read_h()
         read_h = h%x
      end function read_h
   end subroutine host

end module structure_constructor_args_25_mod

program structure_constructor_args_25
   use structure_constructor_args_25_mod
   implicit none
   type(a_t), target :: t
   type(a_t), pointer :: p
   type(a_t) :: arr(2)
   type(o_t) :: o
   type(n_t) :: n
   type(s_t) :: q
   integer :: i

   t%x = 3
   t%y = 4
   t = a_t(x=1, y=t%x)
   if (t%x /= 1 .or. t%y /= 3) error stop 1

   t = a_t(5, 6)
   t = a_t(t%y, t%x)
   if (t%x /= 6 .or. t%y /= 5) error stop 2

   o = o_t(a_t(1, 2), a_t(3, 4), 5)
   o = o_t(a=a_t(10, o%b%x), b=a_t(o%a%x, o%a%y), k=o%a%y)
   if (o%a%x /= 10 .or. o%a%y /= 3) error stop 3
   if (o%b%x /= 1 .or. o%b%y /= 2 .or. o%k /= 2) error stop 4

   o%a = a_t(1, 2)
   o%a = a_t(o%a%y, o%a%x)
   if (o%a%x /= 2 .or. o%a%y /= 1) error stop 5

   arr(1) = a_t(1, 2)
   arr(2) = a_t(3, 4)
   i = 2
   arr(i) = a_t(arr(i)%y, arr(i)%x)
   if (arr(2)%x /= 4 .or. arr(2)%y /= 3) error stop 6
   if (arr(1)%x /= 1 .or. arr(1)%y /= 2) error stop 7

   t = a_t(7, 8)
   p => t
   t = a_t(x=1, y=p%x)
   if (t%x /= 1 .or. t%y /= 7) error stop 8

   t = a_t(1, 1)
   p = a_t(x=5, y=t%x)
   if (t%x /= 5 .or. t%y /= 1) error stop 9

   t = a_t(3, 4)
   n%p => t
   t = a_t(x=7, y=n%p%x)
   if (t%x /= 7 .or. t%y /= 3) error stop 10

   t = a_t(1, 2)
   associate (ax => t%x)
      t = a_t(x=5, y=ax)
   end associate
   if (t%x /= 5 .or. t%y /= 1) error stop 11

   t = a_t(1, 2)
   associate (tt => t)
      tt = a_t(t%y, t%x)
   end associate
   if (t%x /= 2 .or. t%y /= 1) error stop 12

   g = a_t(9, 9)
   g = a_t(x=2, y=get_gx())
   if (g%x /= 2 .or. g%y /= 9) error stop 13

   t = a_t(1, 2)
   call swap(t)
   if (t%x /= 2 .or. t%y /= 1) error stop 14

   call host()

   q = s_t("", [integer ::], 0)
   do i = 1, 3
      q = s_t(s=q%s // "a", v=[q%v, i], n=q%n + 1)
   end do
   if (q%s /= "aaa" .or. q%n /= 3) error stop 15
   if (size(q%v) /= 3) error stop 16
   if (any(q%v /= [1, 2, 3])) error stop 17
   do i = 1, 2
      q = s_t(n=q%n - 1)
      if (allocated(q%v) .or. allocated(q%s)) error stop 18
   end do
   if (q%n /= 1) error stop 19

   print *, "ok"
end program structure_constructor_args_25
