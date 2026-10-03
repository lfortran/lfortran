! Test: associate block with reshape in submodule procedure
! Regression test for ICE in pass_array_by_data when duplicating a
! submodule procedure containing an associate block with reshape.
module submodule_41_m
  implicit none
  interface
    module subroutine make(a)
      integer, intent(in) :: a(:)
    end subroutine
  end interface
end module

submodule(submodule_41_m) submodule_41_s
  implicit none
contains
  module procedure make
    integer :: k
    real, allocatable :: arr(:,:)
    associate(n => size(a))
      arr = reshape([(real(k), k=1,5)], [5, 1])
    end associate
    if (size(arr, 1) /= 5) error stop
    if (size(arr, 2) /= 1) error stop
    if (abs(arr(3, 1) - 3.0) > 1e-6) error stop
  end procedure
end submodule

program submodule_41
  use submodule_41_m
  implicit none
  integer :: a(3)
  a = [1, 2, 3]
  call make(a)
  print *, "ok"
end program
