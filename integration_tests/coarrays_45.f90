program pdt_co_broadcast
  use iso_fortran_env, only: real64
  implicit none

  integer, parameter :: array_len = 5
  integer, parameter :: num_elements = 3

  ! Declare a Parameterized Derived Type with kind (k) parameter
  type :: parameterized_type(k)
    integer, kind :: k
    real(kind=k) :: array_comp(array_len)
    integer :: scalar_comp
  end type parameterized_type

  ! Initialize a 3-element 1D array of the PDT
  type(parameterized_type(real64)) :: pdt_array(num_elements)

  integer :: i, j

  ! Assign values on image 1, and zero out on other images for validation
  if (this_image() == 1) then
    do i = 1, num_elements
      pdt_array(i)%array_comp = [(real(i * 10 + j, kind=real64), j=1, array_len)]
      pdt_array(i)%scalar_comp = i * 100
    end do
  else
    do i = 1, num_elements
      pdt_array(i)%array_comp = 0.0_real64
      pdt_array(i)%scalar_comp = 0
    end do
  end if
  ! Broadcast the array of PDTs from image 1 to all other images
  call co_broadcast(pdt_array, source_image=1)

  ! Validate that all images received the correct values
  do i = 1, num_elements
    if (any(abs(pdt_array(i)%array_comp - [(real(i * 10 + j, kind=real64), j=1, array_len)]) > 1.0e-8_real64)) then
      error stop "bad array_comp value"
    end if

    if (pdt_array(i)%scalar_comp /= i * 100) then
      error stop "bad scalar_comp value"
    end if
  end do

end program pdt_co_broadcast