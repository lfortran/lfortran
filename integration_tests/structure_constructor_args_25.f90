! The value of an intrinsic assignment is evaluated before the target is
! defined (F2018 10.2.1.3). A structure constructor whose arguments read the
! target, directly or through a pointer, an associate name, or a function,
! must therefore see the target's old components, not the ones an earlier
! argument has already given a new value. Building the value in a temporary
! must not add a copy where none is needed either: a component's defined
! assignment or a final procedure would run once more. The last cases use a
! type declared in the main program, assigned in its internal procedures.
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

   type :: b_t
      integer :: v = 0
   contains
      procedure :: asg_b
      generic :: assignment(=) => asg_b
   end type b_t

   type :: c_t
      type(b_t) :: b
      integer :: y = 0
   end type c_t

   type :: f_t
      integer :: x = 0
      integer :: y = 0
   contains
      final :: fin_f
   end type f_t

   type(a_t) :: g
   type(a_t), target :: gt
   integer :: nasg = 0
   integer :: nfin = 0

contains

   subroutine asg_b(lhs, rhs)
      class(b_t), intent(out) :: lhs
      type(b_t), intent(in) :: rhs
      nasg = nasg + 1
      lhs%v = rhs%v + 100
   end subroutine asg_b

   subroutine fin_f(s)
      type(f_t), intent(inout) :: s
      nfin = nfin + 1
   end subroutine fin_f

   integer function seven()
      seven = 7
   end function seven

   subroutine swap_targets(a, b)
      type(a_t), target, intent(inout) :: a, b
      a = a_t(b%y, b%x)
   end subroutine swap_targets

   subroutine swap_with_gt(a)
      type(a_t), target, intent(inout) :: a
      gt = a_t(a%y, a%x)
   end subroutine swap_with_gt

   subroutine counts()
      type(c_t) :: c
      type(b_t) :: bb
      type(f_t) :: t
      integer :: n0, d1, d2, d3
      bb%v = 1
      c = c_t(bb, 2)
      if (c%b%v /= 101 .or. c%y /= 2 .or. nasg /= 1) error stop 40
      c = c_t(bb, seven())
      if (c%b%v /= 101 .or. c%y /= 7 .or. nasg /= 2) error stop 41
      c = c_t(bb, eight())
      if (c%b%v /= 101 .or. c%y /= 8 .or. nasg /= 3) error stop 42

      n0 = nfin
      t = f_t(1, 2)
      d1 = nfin - n0
      if (d1 < 1) error stop 43
      n0 = nfin
      t = f_t(3, seven())
      d2 = nfin - n0
      if (d2 /= d1) error stop 44
      if (t%x /= 3 .or. t%y /= 7) error stop 45
      n0 = nfin
      t = f_t(4, eight())
      d3 = nfin - n0
      if (d3 /= d1) error stop 46
      if (t%x /= 4 .or. t%y /= 8) error stop 47
   contains
      integer function eight()
         eight = 8
      end function eight
   end subroutine counts

   integer function get_gx()
      get_gx = g%x
   end function get_gx

   subroutine swap(t)
      type(a_t), intent(inout) :: t
      t = a_t(t%y, t%x)
   end subroutine swap

   subroutine host()
      type(a_t) :: h
      type(a_t) :: ha(2)
      h = a_t(1, 2)
      h = a_t(x=10, y=read_h())
      if (h%x /= 10 .or. h%y /= 1) error stop 30
      ha(2) = a_t(3, 4)
      ha(2) = a_t(x=10, y=read_ha())
      if (ha(2)%x /= 10 .or. ha(2)%y /= 3) error stop 33
      h = a_t(5, 6)
      block
         h = a_t(x=10, y=read_h())
      end block
      if (h%x /= 10 .or. h%y /= 5) error stop 34
   contains
      integer function read_h()
         read_h = h%x
      end function read_h
      integer function read_ha()
         read_ha = ha(2)%x
      end function read_ha
   end subroutine host

end module structure_constructor_args_25_mod

program structure_constructor_args_25
   use structure_constructor_args_25_mod
   implicit none
   type :: pa_t
      integer :: x = 0
      integer :: y = 0
   end type pa_t
   type(pa_t) :: u
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

   t = a_t(1, 2)
   call swap_targets(t, t)
   if (t%x /= 2 .or. t%y /= 1) error stop 31

   gt = a_t(3, 4)
   call swap_with_gt(gt)
   if (gt%x /= 4 .or. gt%y /= 3) error stop 32

   call counts()

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

   q = s_t("ab", [4, 5, 6], 0)
   q = s_t(s=q%s(2:2), v=q%v(2:3), n=size(q%v))
   if (q%s /= "b" .or. q%n /= 3) error stop 20
   if (size(q%v) /= 2) error stop 21
   if (any(q%v /= [5, 6])) error stop 22

   call swap_u()
   if (u%x /= 2 .or. u%y /= 1) error stop 33
   call init_u(3)
   if (u%x /= 1 .or. u%y /= 3) error stop 34
   u = pa_t(1, 2)
   u = pa_t(5, read_ux())
   if (u%x /= 5 .or. u%y /= 1) error stop 35

   print *, "ok"

contains

   subroutine swap_u()
      u = pa_t(1, 2)
      u = pa_t(u%y, u%x)
   end subroutine swap_u

   subroutine init_u(k)
      integer, intent(in) :: k
      u = pa_t(1, k)
   end subroutine init_u

   integer function read_ux()
      read_ux = u%x
   end function read_ux

end program structure_constructor_args_25
