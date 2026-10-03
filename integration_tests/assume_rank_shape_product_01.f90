! product(shape(a)) for assumed-rank a must compile (LLVM must not emit <0 x t> vectors).
program assume_rank_shape_product_01
  use iso_c_binding, only: c_int, c_size_t, c_null_ptr, c_ptr
  implicit none
  integer :: x(2, 3)
  x = 1
  call sub_with_product_shape(x)
contains
  subroutine sub_with_product_shape(a)
    type(*), intent(inout), target, contiguous :: a(..)
    interface
      subroutine caf_dummy(a, sz, team) bind(c)
        use iso_c_binding, only: c_size_t, c_ptr
        type(*), intent(inout) :: a(..)
        integer(c_size_t), value :: sz
        type(c_ptr), value :: team
      end subroutine
    end interface
    if (int(product(shape(a)), c_size_t) /= 6_c_size_t) error stop
    call caf_dummy(a, int(product(shape(a)), c_size_t), c_null_ptr)
  end subroutine
end program

subroutine caf_dummy(a, sz, team) bind(c)
  use iso_c_binding, only: c_size_t, c_ptr
  implicit none
  type(*), intent(inout) :: a(..)
  integer(c_size_t), value :: sz
  type(c_ptr), value :: team
  if (sz /= 6_c_size_t) error stop
end subroutine caf_dummy
