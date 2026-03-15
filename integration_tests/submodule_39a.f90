program submodule_39
  use input_output_pair_m, only : input_output_pair_t
  use mini_batch_m, only : mini_batch_t, make_batch
  implicit none

  type(input_output_pair_t), allocatable :: input_output_pairs(:)
  type(mini_batch_t) :: mini_batch

  allocate(input_output_pairs(2))
  input_output_pairs(1)%input = 1.0
  input_output_pairs(2)%input = 2.0

  mini_batch = make_batch(input_output_pairs)

  if (.not. allocated(mini_batch%input_output_pairs)) error stop 1
  if (size(mini_batch%input_output_pairs) /= 2) error stop 2
  if (abs(mini_batch%input_output_pairs(1)%input - 1.0) > 1e-6) error stop 3
  if (abs(mini_batch%input_output_pairs(2)%input - 2.0) > 1e-6) error stop 4
end program submodule_39
