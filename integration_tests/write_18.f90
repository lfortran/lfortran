module write_18_mod

   type :: MyType
      integer :: a, b
   end type MyType

   interface write(formatted)
      procedure :: output
   end interface write(formatted)

contains

   subroutine output(self, unit, iotype, v_list, iostat, iomsg)
      class(MyType), intent(in)    :: self
      integer,       intent(in)    :: unit
      character(*),  intent(in)    :: iotype
      integer,       intent(in)    :: v_list(:)
      integer,       intent(out)   :: iostat
      character(*),  intent(inout) :: iomsg

      ! The iotype must be "DT", not "(dt)" or any other form
      if (iotype /= 'DT') error stop
      if (self%a /= 10) error stop
      if (self%b /= 20) error stop
      iostat = 0
   end subroutine output

end module write_18_mod

program write_18
   use write_18_mod
   type(MyType) :: obj
   obj%a = 10
   obj%b = 20
   write(*, '(dt)') obj
   print *, "ok"
end program write_18
