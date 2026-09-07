module extended_type_mod
  use iso_fortran_env, only: real64
  implicit none

  integer, parameter :: array_len = 5

  ! Define the abstract base type
  type, abstract :: base_type
    real(kind=real64) :: base_array(array_len)
  contains
    ! Deferred type-bound procedure requires an abstract interface
    procedure(print_sum_interface), deferred :: print_sum
  end type base_type

  ! Abstract interface for the deferred type-bound procedure
  abstract interface
    subroutine print_sum_interface(this)
      import :: base_type
      class(base_type), intent(in) :: this
    end subroutine print_sum_interface
  end interface

  ! Define the extended derived type inheriting from the abstract base_type
  type, extends(base_type) :: extended_type
    integer :: scalar_comp
  contains
    ! Provide the implementation for the deferred procedure
    procedure :: print_sum => print_sum_impl
  end type extended_type

contains

  subroutine print_sum_impl(this)
    class(extended_type), intent(in) :: this
    print *, "Image ", this_image(), " Element sum: ", sum(this%base_array)
  end subroutine print_sum_impl

end module extended_type_mod


program abstract_type_co_broadcast
  use iso_fortran_env, only: real64
  use extended_type_mod
  implicit none

  integer, parameter :: num_elements = 3
  
  ! Initialize a 3-element 1D array of the extended type
  type(extended_type) :: ext_array(num_elements)
  
  integer :: i, j

  ! Assign values on image 1, and zero out on other images for validation
  if (this_image() == 1) then
    do i = 1, num_elements
      ! Array expression for the inherited base type component
      ext_array(i)%base_array = [(real(i * 10 + j, kind=real64), j=1, array_len)]
      
      ! Assignment for the extended type component
      ext_array(i)%scalar_comp = i * 100
    end do
  else
    do i = 1, num_elements
      ext_array(i)%base_array = 0.0_real64
      ext_array(i)%scalar_comp = 0
    end do
  end if

  ! Broadcast the array of extended types from image 1 to all other images
  call co_broadcast(ext_array, source_image=1)

  ! Validate that all images received the correct values
  ! Debug print on image 2
  if (this_image() == 2) then
    do i = 1, num_elements
      print *, "Image 2, element", i, "base_array:", ext_array(i)%base_array
      print *, "Image 2, element", i, "scalar_comp:", ext_array(i)%scalar_comp
      
      ! Validate inherited array component using array expressions and ANY intrinsic
      if (any(abs(ext_array(i)%base_array - [(real(i * 10 + j, kind=real64), j=1, array_len)]) > 1.0e-8_real64)) then
        error stop "Data corruption detected: base_array elements do not match expected values."
      end if
    
      ! Validate extended scalar component
      if (ext_array(i)%scalar_comp /= i * 100) then
        error stop "Data corruption detected: scalar_comp does not match expected value."
      end if
    end do
  end if

  ! Invoke the type-bound procedure from every image after the broadcast
  call ext_array(1)%print_sum()

end program abstract_type_co_broadcast