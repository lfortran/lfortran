submodule (error_separate_compilation_27) estop_separate_compilation_27
  implicit none

contains

  module subroutine error_sub(code)
    integer, intent(inout) :: code
    code = code + 2
  end subroutine error_sub

end submodule estop_separate_compilation_27