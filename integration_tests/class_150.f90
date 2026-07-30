! Tests a non-pointer, non-allocatable class array dummy argument.
!
! declare_vars() must not treat these as scalar class variables: extract_type()
! strips Array along with Allocatable/Pointer, so a class array would otherwise
! take the scalar-class path and index field 1 of what is actually an array
! descriptor.
program class_150
  implicit none

  type :: t
    integer :: i
  end type t

  type, extends(t) :: u
    integer :: j
  end type u

  class(t), allocatable :: a(:)

  allocate(a(3), source=t(5))
  call check(a)

  ! same dummy, but with a dynamic type extending the declared one
  deallocate(a)
  allocate(a(2), source=u(1, 2))
  call check(a)

contains

  subroutine check(arg)
    class(t) :: arg(:)
    integer :: k
    do k = 1, size(arg)
      print *, arg(k)%i
    end do
  end subroutine check

end program class_150
