module typed_allocation

   type :: Base
      real :: data = 2.0
   end type Base
   
   type, extends(Base) :: Extended
   end type Extended
   
contains
   
   subroutine allocator(array)
      class(Base), allocatable, intent(out) :: array(:)
      allocate( Extended :: array(1) )
   end subroutine allocator

end module typed_allocation

program test

   use typed_allocation

   class(Base), allocatable :: array(:)

   call allocator(array)

!    print *, array(1)%data
   
end program test
