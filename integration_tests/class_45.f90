module class_45_mod

   type, public, abstract :: AbsType
   contains
      procedure(method), deferred, nopass :: method
   end type AbsType

   abstract interface
      function method() result(arr)
         import
         integer, allocatable :: arr(:)
      end function method
   end interface

   type, extends(AbsType) :: ConcreteType
   contains
      procedure, nopass :: method => concrete_method
   end type ConcreteType

contains

   subroutine client(obj)
      class(AbsType), intent(in) :: obj
      integer, allocatable :: local_arr(:)
      allocate(local_arr(2))
      
      local_arr = obj%method()
      print *, "local_arr: ", local_arr

      if (.not. all(local_arr == [1, 2])) error stop
   end subroutine client

   function concrete_method() result(arr)
      integer, allocatable :: arr(:)
      allocate(arr(2))
      arr = [1, 2]
   end function concrete_method

end module class_45_mod

program class_45
   use class_45_mod
   implicit none

   class(AbsType), allocatable :: var

   allocate(ConcreteType :: var)
   
   call client(var)

end program class_45
