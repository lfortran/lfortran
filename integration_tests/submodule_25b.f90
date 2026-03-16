module submodule_25_m
  implicit none
  type :: op_t
  contains
    generic :: operator(.x.) => apply
    procedure, private :: apply
  end type
  type :: tensor_t
    integer :: n_
    real(8), allocatable :: data_(:)
  end type
  interface tensor_t
    module function make_tensor(data, n) result(r)
      real(8), intent(in) :: data(:)
      integer, intent(in) :: n
      type(tensor_t) :: r
    end function
  end interface
  interface
    module function apply(self, vec) result(out)
      class(op_t), intent(in) :: self
      real(8), intent(in) :: vec(:)
      real(8), allocatable :: out(:)
    end function
    module function compute() result(t)
      type(tensor_t) :: t
    end function
  end interface
end module
