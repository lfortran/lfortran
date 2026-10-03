module m_dtio_child
   implicit none
   type :: t
   contains
      procedure, private :: write_t
      generic, public :: write(formatted) => write_t
   end type t
contains
   subroutine write_t(self, unit, iotype, v_list, iostat, iomsg)
      class(t), intent(in) :: self
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
      iostat = 0
      ! Child writes should share the parent record (no extra newlines)
      write(unit, '(a)') 'AB'
      write(unit, '(a)') 'CD'
   end subroutine write_t
end module m_dtio_child

program main
   use m_dtio_child, only: t
   implicit none
   type(t) :: x
   character(len=100) :: line

   ! Write to a file and read back to verify output
   open(unit=20, file='_dtio_test_139.txt', status='replace')
   write(20, '(dt)') x
   close(20)

   open(unit=20, file='_dtio_test_139.txt', status='old')
   read(20, '(a)') line
   close(20, status='delete')

   if (trim(line) /= 'ABCD') error stop
   print *, "OK"
end program main
