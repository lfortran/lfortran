module separate_compilation_42_kind_m
  implicit none

  integer, parameter :: default_real = kind(1.0)
  integer, parameter :: double_precision = kind(1.0d0)
end module separate_compilation_42_kind_m

module separate_compilation_42_hyperparameters_m
  use separate_compilation_42_kind_m, only : default_real, double_precision
  implicit none

  type hyperparameters_t(k)
    integer, kind :: k = default_real
    integer :: mini_batches_ = 10
    real(k) :: learning_rate_ = real(1.5, k)
    complex(k) :: damping_ = (0.5, 0.25)
  end type hyperparameters_t

  interface hyperparameters_t
    pure module function default_real_from_components(mini_batches, learning_rate, damping) result(hyperparameters)
      integer, intent(in) :: mini_batches
      real, intent(in) :: learning_rate
      complex, intent(in) :: damping
      type(hyperparameters_t) :: hyperparameters
    end function default_real_from_components

    pure module function double_precision_from_components(mini_batches, learning_rate, damping) result(hyperparameters)
      integer, intent(in) :: mini_batches
      double precision, intent(in) :: learning_rate
      complex(double_precision), intent(in) :: damping
      type(hyperparameters_t(double_precision)) :: hyperparameters
    end function double_precision_from_components
  end interface
end module separate_compilation_42_hyperparameters_m

submodule(separate_compilation_42_hyperparameters_m) separate_compilation_42_hyperparameters_s
contains

  module procedure default_real_from_components
    hyperparameters%mini_batches_ = mini_batches
    hyperparameters%learning_rate_ = learning_rate
    hyperparameters%damping_ = damping
  end procedure default_real_from_components

  module procedure double_precision_from_components
    hyperparameters%mini_batches_ = mini_batches
    hyperparameters%learning_rate_ = learning_rate
    hyperparameters%damping_ = damping
  end procedure double_precision_from_components

end submodule separate_compilation_42_hyperparameters_s

program separate_compilation_42
  use separate_compilation_42_hyperparameters_m, only : hyperparameters_t
  use separate_compilation_42_kind_m, only : double_precision
  implicit none

  type(hyperparameters_t) :: default_hyperparameters
  type(hyperparameters_t(double_precision)) :: double_hyperparameters
  type(hyperparameters_t) :: defaults_only
  type(hyperparameters_t(double_precision)) :: dp_defaults_only

  default_hyperparameters = hyperparameters_t(4, 0.25, (0.3, 0.4))
  double_hyperparameters = hyperparameters_t(6, 0.125d0, (0.5d0, 0.6d0))

  if (default_hyperparameters%mini_batches_ /= 4) error stop
  if (abs(default_hyperparameters%learning_rate_ - 0.25) > epsilon(1.0)) error stop
  if (abs(real(default_hyperparameters%damping_) - 0.3) > epsilon(1.0)) error stop
  if (abs(aimag(default_hyperparameters%damping_) - 0.4) > epsilon(1.0)) error stop
  if (double_hyperparameters%mini_batches_ /= 6) error stop
  if (abs(double_hyperparameters%learning_rate_ - 0.125d0) > epsilon(1.0d0)) error stop
  if (abs(real(double_hyperparameters%damping_) - 0.5d0) > epsilon(1.0d0)) error stop
  if (abs(aimag(double_hyperparameters%damping_) - 0.6d0) > epsilon(1.0d0)) error stop

  ! Verify complex default initializers (kind 4 and kind 8)
  if (abs(real(defaults_only%damping_) - 0.5) > epsilon(1.0)) error stop
  if (abs(aimag(defaults_only%damping_) - 0.25) > epsilon(1.0)) error stop
  if (abs(real(dp_defaults_only%damping_) - 0.5d0) > epsilon(1.0d0)) error stop
  if (abs(aimag(dp_defaults_only%damping_) - 0.25d0) > epsilon(1.0d0)) error stop
end program separate_compilation_42
