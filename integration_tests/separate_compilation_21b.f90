submodule (math_separate_compilation_21) log_separate_compilation_21
  implicit none

contains

  module procedure func1
    real, parameter :: array(2) = [1.0 , 2.0]
    res = array
  end procedure

  module procedure func2
    integer, parameter :: array(2) = [1 , 2]
    res = array
  end procedure

end submodule