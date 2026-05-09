module nested
contains
   subroutine outer(x)
      real(8), allocatable, intent(in) :: x(:,:)
      print *, 'outer: lbound(x,1) is ', lbound(x,1)
      print *, 'outer: ubound(x,1) is ', ubound(x,1)
      call inner()
   contains
      subroutine inner()
         print *, 'inner: lbound(x,1) is ', lbound(x,1)
         print *, 'inner: ubound(x,1) is ', ubound(x,1)
      end subroutine inner      
   end subroutine outer
end module nested

program main
   use nested
   implicit none
   real(8), dimension(:,:), allocatable :: x
   allocate( x(-4:44,3) )
   call outer(x)
end program main
