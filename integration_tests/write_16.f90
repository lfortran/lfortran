module write_16_test

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

end module write_16_test

program write_16
   use write_16_test
   type(MyType) :: x
   integer :: u
   character(len=100) :: line

   open(newunit=u, file="write_16.txt", status="replace")
   write(u,*) x
   close(u)

   open(newunit=u, file="write_16.txt", status="old")
   read(u,'(A)') line
   close(u)

   if (trim(adjustl(line)) /= "MyType instance") then
      error stop "Unexpected output from write(formatted)"
   end if
end program write_16
