module write_45_mod
   implicit none
   type :: t
      integer :: i = 0
   end type t
   character(len=16) :: last_iotype
   integer :: last_n
   integer :: last_v_list(10)
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
end module write_45_mod

program write_45
   use write_45_mod
   implicit none
   type(t) :: x
   integer :: u

   open (newunit=u, status="scratch")

   write (u, "(dt(5))") x
   if (last_iotype /= "DT") error stop
   if (last_n /= 1) error stop
   if (last_v_list(1) /= 5) error stop

   write (u, "(DT'abc'(1, 20,3))") x
   if (last_iotype /= "DTabc") error stop
   if (last_n /= 3) error stop
   if (any(last_v_list(1:3) /= [1, 20, 3])) error stop

   write (u, "(dt'it''s')") x
   if (last_iotype /= "DTit's") error stop
   if (last_n /= 0) error stop

   write (u, "(dt)") x
   if (last_iotype /= "DT") error stop
   if (last_n /= 0) error stop

   print "(dt'p'(7,8))", x
   if (last_iotype /= "DTp") error stop
   if (last_n /= 2) error stop
   if (any(last_v_list(1:2) /= [7, 8])) error stop

   rewind (u)
   read (u, "(dt'r'(4))") x
   if (last_iotype /= "DTr") error stop
   if (last_n /= 1) error stop
   if (last_v_list(1) /= 4) error stop

   close (u)
end program write_45
