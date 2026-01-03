module class_50_m_1
   private

   type, public, abstract :: Addition
   contains
      procedure(addto), deferred :: addto
   end type Addition

   abstract interface
      subroutine addto(self,a)
         import
         class(Addition), intent(in)    :: self
         integer,         intent(inout) :: a
      end subroutine addto
   end interface

end module class_50_m_1

module class_50_m_2
   use class_50_m_1, only: Addition
   private

   type, public, extends(Addition) :: AdderClass
   contains
      procedure :: addto
   end type AdderClass

contains

   subroutine addto(self,a)
      class(AdderClass), intent(in)    :: self
      integer,           intent(inout) :: a
      a = a + 3
   end subroutine addto

end module class_50_m_2

program class_50

   use class_50_m_1,    only: Addition
   use class_50_m_2,    only: AdderClass

   integer :: a
   class(Addition), allocatable :: adder

   a = 1
   adder = AdderClass()

   call adder%addto(a)

   if (a /= 4) error stop
end program class_50
