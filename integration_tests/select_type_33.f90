! Test Functions having select type blocks with custom types selection
module select_33_mod
   implicit none

   type, abstract :: AbsType
   end type AbsType
   
   type, extends(AbsType) :: MyType
      real(8), allocatable :: arr(:,:)
   end type MyType

   contains
   subroutine reshaper(obj, n)
      class(AbsType), intent(in) :: obj
      integer,        intent(in) :: n(:)      
      real(8), dimension(n(1)*n(2)) :: a
      integer :: s(1)
      
      select type (obj)
      type is (MyType)
         ! Reshape the 2D slice into the 1D local array 'a'
         a = reshape(obj%arr(1:n(1), 1:n(2)), [n(1)*n(2)])
         s = shape(a)
         print *, s 
         print *, a
         if ((s(1)) /= 6) error stop
         if ((a(1) - 1.0) >= 1e-4) error stop
         if ((a(2) - 2.0) >= 1e-4) error stop
         if ((a(3) - 3.0) >= 1e-4) error stop
         if ((a(4) - 4.0) >= 1e-4) error stop
         if ((a(5) - 5.0) >= 1e-4) error stop
         if ((a(6) - 6.0) >= 1e-4) error stop
      class default
         print *, "Unknown type."
         error stop
      end select
   end subroutine reshaper

end module select_33_mod

program select_33
    use select_33_mod
    implicit none
    
    type(MyType) :: instance
    integer      :: dims(2)
    integer      :: i, j
    dims = [3, 2]

    allocate(instance%arr(dims(1), dims(2)))

    instance%arr = reshape([(real(i, 8), i = 1, 6)], [3, 2])
    call reshaper(instance, dims)
end program select_33