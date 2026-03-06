module separate_compilation_40_tensor_m
  implicit none

  type tensor_t(k)
    integer, kind :: k = kind(1.0)
    real(k), allocatable, private :: values(:)
  contains
    procedure, private :: get_values
  end type tensor_t

  interface
    pure module function get_values(self) result(v)
      class(tensor_t), intent(in) :: self
      real, allocatable :: v(:)
    end function get_values
  end interface
end module separate_compilation_40_tensor_m

submodule(separate_compilation_40_tensor_m) separate_compilation_40_tensor_s
  implicit none
contains
  module procedure get_values
    v = self%values
  end procedure get_values
end submodule separate_compilation_40_tensor_s

module separate_compilation_40_pair_m
  use separate_compilation_40_tensor_m, only : tensor_t
  implicit none

  type pair_t(k)
    integer, kind :: k = kind(1.0)
    type(tensor_t(k)), private :: x
  contains
    procedure, private :: input
  end type pair_t

  interface
    pure module function input(self) result(v)
      class(pair_t), intent(in) :: self
      type(tensor_t) :: v
    end function input

    module subroutine shuffle(pairs)
      type(pair_t), intent(inout) :: pairs(:)
    end subroutine shuffle
  end interface
end module separate_compilation_40_pair_m

submodule(separate_compilation_40_pair_m) separate_compilation_40_pair_s
  implicit none
contains
  module procedure input
    v = self%x
  end procedure input

  module procedure shuffle
    type(pair_t) :: tmp

    tmp = pairs(1)
    pairs(1) = pairs(2)
    pairs(2) = tmp
  end procedure shuffle
end submodule separate_compilation_40_pair_s

program separate_compilation_40
  implicit none

  print *, "PASS"
end program separate_compilation_40
