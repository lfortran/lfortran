module write_19_mod
   implicit none
   type :: t
      integer :: x = 42
   contains
      procedure, private :: write_t
      generic :: write(formatted) => write_t
   end type t
contains
   subroutine write_t(self, unit, iotype, v_list, iostat, iomsg)
      class(t), intent(in) :: self
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
      if (iotype /= 'DT') error stop
      if (self%x /= 42) error stop
      iostat = 0
   end subroutine
end module

module write_19_io
   use write_19_mod, only: t
   implicit none
contains
   subroutine print_it(obj)
      class(t), intent(in) :: obj
      write(*,'(dt)') obj
   end subroutine
end module
