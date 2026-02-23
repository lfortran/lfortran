module block_11_mod
   implicit none

   type, abstract :: AbsType
   contains
      procedure(nelements), deferred, nopass :: nelements
   end type AbsType

   abstract interface
      pure function nelements() result(n)
         integer :: n
      end function nelements
   end interface

   type, extends(AbsType) :: ConcreteType
   contains
      procedure, nopass :: nelements => concrete_nelements
   end type ConcreteType

   type :: MyType
      class(AbsType), allocatable :: obj
   contains
      procedure :: caller
   end type MyType

contains

   pure function concrete_nelements() result(n)
      integer :: n
      n = 3
   end function concrete_nelements

   subroutine caller(self)
      class(MyType), intent(in) :: self
      block
        real(8), dimension(self%obj%nelements()) :: a
        integer :: i
        do i = 1, size(a)
           a(i) = real(i, 8)
        end do
        if (size(a) /= 3) error stop
        if (abs(a(1) - 1.0d0) > 1.0d-12) error stop
        if (abs(a(3) - 3.0d0) > 1.0d-12) error stop
      end block
   end subroutine caller

end module block_11_mod

program block_11
   use block_11_mod
   implicit none
   type(MyType) :: m

   allocate(ConcreteType :: m%obj)
   call m%caller()
   print *, "ok"
end program block_11
