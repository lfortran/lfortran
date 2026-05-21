module pass_array_by_data_14_mod
  implicit none

  type :: padding_holder_type
     integer, dimension(2) :: padding_2d
  end type padding_holder_type

  interface padding_holder_type
     module function layer_setup(padding) result(layer)
       integer, dimension(:), intent(in) :: padding
       type(padding_holder_type) :: layer
     end function layer_setup
  end interface padding_holder_type

contains

  module function layer_setup(padding) result(layer)
    integer, dimension(:), intent(in) :: padding
    type(padding_holder_type) :: layer
    integer, dimension(2) :: padding_2d

    select case(size(padding))
    case(1)
       padding_2d = [padding(1), padding(1)]
    case(2)
       padding_2d = padding
    case default
       error stop "Invalid padding size"
    end select
    layer%padding_2d = padding_2d
  end function layer_setup

end module pass_array_by_data_14_mod


program pass_array_by_data_14
  use pass_array_by_data_14_mod
  implicit none
  integer, dimension(3, 2) :: test_paddings
  integer :: i
  type(padding_holder_type) :: layer

  test_paddings(1,:) = [0, 0]
  test_paddings(2,:) = [1, 2]
  test_paddings(3,:) = [2, 1]

  do i = 1, 3
     layer = padding_holder_type(padding = test_paddings(i,:))
     if (any(layer%padding_2d /= test_paddings(i,:))) then
        print *, "i=", i, " expected=", test_paddings(i,:), " got=", layer%padding_2d
        error stop
     end if
  end do
end program pass_array_by_data_14
