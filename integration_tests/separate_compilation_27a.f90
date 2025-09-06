module kinds_separate_compilation_27
  use iso_fortran_env, only: int8, int16, int32, int64
end module kinds_separate_compilation_27


module error_separate_compilation_27
  implicit none

  interface 
    module subroutine error_sub(code)
      integer, intent(inout) :: code
    end subroutine error_sub
  end interface

end module error_separate_compilation_27


module io_separate_compilation_27

  use kinds_separate_compilation_27, only: int8, int16, int32, int64
  use error_separate_compilation_27, only: error_sub
  implicit none

contains

  subroutine open(i)
    integer, intent(inout) :: i
    call error_sub(i)
  end subroutine open

end module io_separate_compilation_27