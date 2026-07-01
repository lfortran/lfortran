module nested
contains
   subroutine outer(x)
      real(8), allocatable, intent(in) :: x(:,:)
      print *, 'outer: lbound(x,1) is ', lbound(x,1)
      if (lbound(x,1) /= -4) error stop
      print *, 'outer: ubound(x,1) is ', ubound(x,1)
      if (ubound(x,1) /= 44) error stop
      call inner()
   contains
      subroutine inner()
         print *, 'inner: lbound(x,1) is ', lbound(x,1)
         if (lbound(x,1) /= -4) error stop
         print *, 'inner: ubound(x,1) is ', ubound(x,1)
         if (ubound(x,1) /= 44) error stop
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
