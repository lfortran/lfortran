module assert_m_module_associate_20

public :: assert_this_image_interface, assert_this_image

abstract interface
  pure function assert_this_image_interface() result(this_image_id)
    integer :: this_image_id
  end function
end interface
procedure(assert_this_image_interface), pointer :: assert_this_image

contains

  subroutine assert_always(is_two)
    logical, intent(in) :: is_two
    print *, assert_this_image()
    if (is_two) then
      if ( assert_this_image() /= 112352 ) error stop
    else
      if ( assert_this_image() /= 112351 ) error stop
    end if
  end subroutine

end module assert_m_module_associate_20


program associate_20
  use assert_m_module_associate_20
  implicit none

  assert_this_image => assert_callback_this_image
  call assert_always(.false.)
  assert_this_image => assert_callback_this_image_two
  call assert_always(.true.)

contains
  
  pure function assert_callback_this_image() result(this_image_id)
    implicit none
    integer :: this_image_id
    this_image_id = 112351
  end function

  pure function assert_callback_this_image_two() result(this_image_id)
    implicit none
    integer :: this_image_id
    this_image_id = 112352
  end function
end program
