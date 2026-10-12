! The DT edit descriptor's character literal and v-list are passed to the
! defined input/output procedure in its iotype and v_list arguments.
module write_50_mod
   implicit none
   type :: t
      integer :: i = 0
   end type t
   character(len=16) :: last_iotype = ""
   integer :: last_n = -1
   integer :: last_v_list(10) = 0
   interface write (formatted)
      module procedure wf
   end interface
   interface read (formatted)
      module procedure rf
   end interface
contains
   subroutine wf(dtv, unit, iotype, v_list, iostat, iomsg)
      class(t), intent(in) :: dtv
      integer, intent(in) :: unit, v_list(:)
      character(len=*), intent(in) :: iotype
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
      last_iotype = iotype
      last_n = size(v_list)
      last_v_list(1:last_n) = v_list
      iostat = 0
   end subroutine wf

   subroutine rf(dtv, unit, iotype, v_list, iostat, iomsg)
      class(t), intent(inout) :: dtv
      integer, intent(in) :: unit, v_list(:)
      character(len=*), intent(in) :: iotype
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
      last_iotype = iotype
      last_n = size(v_list)
      last_v_list(1:last_n) = v_list
      iostat = 0
   end subroutine rf
end module write_50_mod

program write_50
   use write_50_mod
   implicit none
   character(len=*), parameter :: fmt = "(DT'par'(9,10))"
   type(t) :: x
   integer :: u

   open (newunit=u, status="scratch")

   write (u, "(dt(5))") x
   if (last_iotype /= "DT") error stop 1
   if (last_n /= 1) error stop 2
   if (last_v_list(1) /= 5) error stop 3

   write (u, "(DT'abc'(1, 20,3))") x
   if (last_iotype /= "DTabc") error stop 4
   if (last_n /= 3) error stop 5
   if (any(last_v_list(1:3) /= [1, 20, 3])) error stop 6

   write (u, "(dt'it''s')") x
   if (last_iotype /= "DTit's") error stop 7
   if (last_n /= 0) error stop 8

   write (u, '(dt"q()")') x
   if (last_iotype /= "DTq()") error stop 9
   if (last_n /= 0) error stop 10

   write (u, "(dt)") x
   if (last_iotype /= "DT") error stop 11
   if (last_n /= 0) error stop 12

   write (u, fmt) x
   if (last_iotype /= "DTpar") error stop 13
   if (last_n /= 2) error stop 14
   if (any(last_v_list(1:2) /= [9, 10])) error stop 15

   print "(dt'p'(7,8))", x
   if (last_iotype /= "DTp") error stop 16
   if (last_n /= 2) error stop 17
   if (any(last_v_list(1:2) /= [7, 8])) error stop 18

   rewind (u)
   read (u, "(dt'r'(4))") x
   if (last_iotype /= "DTr") error stop 19
   if (last_n /= 1) error stop 20
   if (last_v_list(1) /= 4) error stop 21

   rewind (u)
   read (u, "(dt(2,3))") x
   if (last_iotype /= "DT") error stop 22
   if (last_n /= 2) error stop 23
   if (any(last_v_list(1:2) /= [2, 3])) error stop 24

   close (u)
   print *, "ok"
end program write_50
