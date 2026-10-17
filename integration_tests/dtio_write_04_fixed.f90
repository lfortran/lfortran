C     Defined output with the DT edit descriptor in fixed-form FORMAT
C     statements, with blanks inside the descriptor and a continuation line.
      module dtio_write_04_fixed_mod
      implicit none
      character(len=16) :: last_iotype = ''
      integer :: last_n = -1
      integer :: last_v(4) = 0

      type :: t
          integer :: v = 0
      end type

      interface write(formatted)
      module procedure wf
      end interface

      contains

      subroutine wf(dtv, unit, iotype, v_list, iostat, iomsg)
      class(t), intent(in) :: dtv
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
      last_iotype = iotype
      last_n = size(v_list)
      last_v = 0
      last_v(1:size(v_list)) = v_list
      write(unit, '("<",a,":",i0,":",*(i0,:,","))', iostat=iostat)
     &    iotype, dtv%v, v_list
      end subroutine

      end module

      program dtio_write_04_fixed
      use dtio_write_04_fixed_mod
      implicit none
      type(t) :: x, y
      integer :: out
      character(len=64) :: line
   10 format(dt,1x,dt)
      x%v = 1
      y%v = 2
      open(newunit=out, status='scratch', action='readwrite',
     &    form='formatted')
      write(out, 10) x, y
      write(out, 20) x
      if (last_iotype /= 'DTx') error stop 1
      if (last_n /= 1 .or. last_v(1) /= 2) error stop 2
      write(out, 30) 5, y
      if (last_iotype /= 'DTA b') error stop 3
      if (last_n /= 2) error stop 4
      if (last_v(1) /= 1 .or. last_v(2) /= 2) error stop 5
      print 10, x, y
   20 format(d t'x'( 2 ))
   30 format(i0,1x,DT"A b"(1,
     &2))
      rewind(out)
      read(out, '(a)') line
      if (line /= '<DT:1: <DT:2:') error stop 6
      read(out, '(a)') line
      if (line /= '<DTx:1:2') error stop 7
      read(out, '(a)') line
      if (line /= '5 <DTA b:2:1,2') error stop 8
      close(out)
      print *, 'ok'
      end program
