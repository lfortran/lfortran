module gpu_metal_391_mod
  ! The routines of gpu_metal_391, with their bodies in a submodule that is
  ! compiled separately from the loop that calls them.
  implicit none
  type :: net_t
    integer, allocatable :: nodes_(:)
  contains
    procedure :: infer
  end type
  interface
    pure module function layer_sum(n, x) result(r)
      integer, intent(in) :: n
      real, intent(in) :: x
      real :: r
    end function
    elemental module function infer(self, x) result(y)
      class(net_t), intent(in) :: self
      real, intent(in) :: x
      real :: y
    end function
  end interface
end module

submodule (gpu_metal_391_mod) gpu_metal_391_sub
  implicit none
contains
  module procedure layer_sum
    real, allocatable :: a(:)
    integer :: i
    allocate(a(n))
    do i = 1, n
      a(i) = x + i
    end do
    r = sum(a)
  end procedure

  module procedure infer
    real, allocatable :: w(:)
    allocate(w(size(self%nodes_) + 2))
    w = 1
    associate (s => layer_sum(size(self%nodes_) + 2, x))
      y = sum(w) + s
    end associate
  end procedure
end submodule
