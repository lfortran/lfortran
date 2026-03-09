module input_output_pair_m
  implicit none

  type input_output_pair_t(k)
    integer, kind :: k = kind(1.0)
    real(k) :: input
  end type input_output_pair_t
end module input_output_pair_m

module mini_batch_m
  use input_output_pair_m, only : input_output_pair_t
  implicit none

  type mini_batch_t(k)
    integer, kind :: k = kind(1.0)
    type(input_output_pair_t(k)), allocatable :: input_output_pairs(:)
  end type mini_batch_t

  interface
    pure module function make_batch(input_output_pairs) result(mini_batch)
      type(input_output_pair_t), intent(in) :: input_output_pairs(:)
      type(mini_batch_t) :: mini_batch
    end function make_batch
  end interface
end module mini_batch_m
