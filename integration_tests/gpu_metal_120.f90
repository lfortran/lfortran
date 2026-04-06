module gpu_metal_120_m
  implicit none

  type :: network_t
    real, allocatable :: weights_(:)
    integer, allocatable :: nodes_(:)
  contains
    procedure :: num_hidden_layers
  end type

contains

  elemental integer function num_hidden_layers(self) result(count)
    class(network_t), intent(in) :: self
    count = size(self%nodes_) - 2
  end function

  subroutine train(self, n)
    type(network_t), intent(in) :: self
    integer, intent(in) :: n
    integer :: i
    integer :: result(4)

    result = 0

    do concurrent (i = 1:n)
      result(i) = self%num_hidden_layers()
    end do

    do i = 1, n
      if (result(i) /= 1) error stop
    end do
    print *, "PASS"
  end subroutine

end module

program gpu_metal_120
  use gpu_metal_120_m
  implicit none
  type(network_t) :: net

  allocate(net%nodes_(0:2), source=[2, 3, 1])
  allocate(net%weights_(3), source=1.0)

  call train(net, 4)
end program
