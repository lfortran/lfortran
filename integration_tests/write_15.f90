module write_15_test

   type :: MyType
   end type MyType

   interface write(formatted)
      procedure :: output
   end interface write(formatted)

contains

   subroutine output(obj,unit,iotype,v_list,iostat,iomsg)
      class(MyType), intent(in)    :: obj
      integer,       intent(in)    :: unit
      character(*),  intent(in)    :: iotype
      integer,       intent(in)    :: v_list(:)
      integer,       intent(out)   :: iostat
      character(*),  intent(inout) :: iomsg

      iostat = 0
      write(unit,'(A)') 'MyType instance'
   end subroutine output

end module write_15_test

program write_15
   use test
   type(MyType) :: x

   write(*,*) x
end program write_15