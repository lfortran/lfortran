submodule (math_separate_compilation_21) log_separate_compilation_21
  implicit none

contains

  module procedure func
    real, parameter :: array(2) = [1.0 , 2.0]
    res = array
  end procedure

end submodule