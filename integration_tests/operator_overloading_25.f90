module operator_overloading_25_mod
   implicit none

   type, public, abstract :: AbsType
   contains
      procedure(writeout), deferred :: writeout
      generic :: write(formatted) => writeout
   end type AbsType

   abstract interface
      subroutine writeout(self,unit,iotype,v_list,iostat,iomsg)
         import
         class(AbsType), intent(in)    :: self
         integer,        intent(in)    :: unit
         character(*),   intent(in)    :: iotype
         integer,        intent(in)    :: v_list(:)
         integer,        intent(out)   :: iostat
         character(*),   intent(inout) :: iomsg
      end subroutine writeout
   end interface

   type, extends(AbsType) :: MyType
      integer :: value
   contains
      procedure :: writeout => my_write
   end type MyType

   type :: IoType
   contains
      procedure, nopass :: output
   end type IoType

contains

   subroutine my_write(self,unit,iotype,v_list,iostat,iomsg)
      class(MyType), intent(in)       :: self
      integer,       intent(in)       :: unit
      character(*),  intent(in)       :: iotype
      integer,       intent(in)       :: v_list(:)
      integer,       intent(out)      :: iostat
      character(*),  intent(inout)    :: iomsg

      iostat = 0

      ! Basic validation
      if (len_trim(iotype) == 0) then
         iostat = 1
         iomsg = "Invalid iotype"
         return
      end if

      write(unit,'(A,I0)') "MyType value = ", self%value

   end subroutine my_write

   subroutine output(obj)
      class(AbsType), intent(in) :: obj
      integer :: ios
      character(len=200) :: msg

      write(*,'(DT)', iostat=ios, iomsg=msg) obj

      if (ios /= 0) then
         print *, "I/O Error:", trim(msg)
         error stop "DT write failed"
      end if

   end subroutine output

end module operator_overloading_25_mod

program operator_overloading_25
   use operator_overloading_25_mod
   implicit none

   type(MyType) :: x
   type(IoType) :: io

   x%value = 42

   call io%output(x)

   print *, "operator_overloading_25 completed successfully."

end program operator_overloading_25