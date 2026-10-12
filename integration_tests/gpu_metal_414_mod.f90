! The types of gpu_metal_414, as FIATS declares its mini-batches: a batch
! holds an allocatable array of input-output pairs, a pair holds two
! tensors, and a tensor holds an allocatable array. Every constructor is a
! module procedure of a submodule, compiled separately.
module gpu_metal_414_tensor_m
  implicit none
  type tensor_t
    real, allocatable :: values_(:)
  end type
  interface tensor_t
    pure module function construct(values) result(tensor)
      implicit none
      real, intent(in) :: values(:)
      type(tensor_t) tensor
    end function
  end interface
end module

submodule (gpu_metal_414_tensor_m) gpu_metal_414_tensor_s
  implicit none
contains
  module procedure construct
    tensor%values_ = values
  end procedure
end submodule

module gpu_metal_414_pair_m
  use gpu_metal_414_tensor_m, only : tensor_t
  implicit none
  type pair_t
    type(tensor_t) :: inputs_, outputs_
  end type
  interface pair_t
    elemental module function pair_construct(inputs, outputs) result(pair)
      implicit none
      type(tensor_t), intent(in) :: inputs, outputs
      type(pair_t) pair
    end function
  end interface
end module

submodule (gpu_metal_414_pair_m) gpu_metal_414_pair_s
  implicit none
contains
  module procedure pair_construct
    pair%inputs_ = inputs
    pair%outputs_ = outputs
  end procedure
end submodule

module gpu_metal_414_batch_m
  use gpu_metal_414_pair_m, only : pair_t
  implicit none
  type batch_t
    type(pair_t), allocatable :: pairs_(:)
  end type
  interface batch_t
    pure module function batch_construct(pairs) result(batch)
      implicit none
      type(pair_t), intent(in) :: pairs(:)
      type(batch_t) batch
    end function
  end interface
end module

submodule (gpu_metal_414_batch_m) gpu_metal_414_batch_s
  implicit none
contains
  module procedure batch_construct
    batch%pairs_ = pairs
  end procedure
end submodule
