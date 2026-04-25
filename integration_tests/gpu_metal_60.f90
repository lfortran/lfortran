! Test: gpu_offload pass with submodule type-bound procedure call
! inside do concurrent with nested associate and block constructs.
! Exercises: (1) import_struct_type scope resolution for ExternalSymbols
! in associate blocks, (2) GpuFunctionCollector descending into Block
! bodies, (3) importing submodule function implementations into GPU
! kernel scope.
module gpu_metal_60_activation_mod
  implicit none
  type :: activation_t
    integer :: selection_ = 1
  contains
    procedure, non_overridable, private :: default_real_differentiate
    generic :: differentiate => default_real_differentiate
  end type
  interface
    elemental module function default_real_differentiate(self, x) result(dy_dx)
      class(activation_t), intent(in) :: self
      real, intent(in) :: x
      real :: dy_dx
    end function
  end interface
end module gpu_metal_60_activation_mod

submodule(gpu_metal_60_activation_mod) gpu_metal_60_activation_sub
  implicit none
contains
  module procedure default_real_differentiate
    dy_dx = x * 2.0 + 1.0
  end procedure
end submodule gpu_metal_60_activation_sub

module gpu_metal_60_network_mod
  use gpu_metal_60_activation_mod, only : activation_t
  implicit none
contains
  subroutine train(act, result_arr)
    type(activation_t), intent(in) :: act
    real, intent(inout) :: result_arr(:)
    integer :: pair
    real :: x
    x = 1.0
    associate(b => x)
      do concurrent (pair = 1:2)
        block
          real delta
          associate(y => result_arr)
            delta = act%differentiate(1.0)
          end associate
          result_arr(pair) = delta
        end block
      end do
    end associate
  end subroutine
end module gpu_metal_60_network_mod

program gpu_metal_60
  use gpu_metal_60_network_mod
  use gpu_metal_60_activation_mod, only : activation_t
  implicit none
  type(activation_t) :: act
  real :: results(2)
  results = 0.0
  call train(act, results)
  ! default_real_differentiate(1.0) = 1.0 * 2.0 + 1.0 = 3.0
  if (abs(results(1) - 3.0) > 1.0e-6) error stop
  if (abs(results(2) - 3.0) > 1.0e-6) error stop
  print *, "PASS"
end program gpu_metal_60
