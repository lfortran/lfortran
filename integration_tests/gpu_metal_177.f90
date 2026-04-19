module gpu_metal_177_m
  implicit none
  type :: data_t
    real, allocatable :: v(:)
  end type
contains
  elemental function f(x) result(r)
    real, intent(in) :: x
    real :: r
    r = x * 2.0
  end function
  pure function get_v(d) result(res)
    type(data_t), intent(in) :: d
    real, allocatable :: res(:)
    allocate(res(size(d%v)))
    res = d%v
  end function
  subroutine run(arr, n)
    type(data_t), intent(in) :: arr(:)
    integer, intent(in) :: n
    integer :: i
    real :: z(4), out(4)
    z = 1.0
    out = 0.0
    do concurrent (i = 1:size(arr))
      associate(y => get_v(arr(i)))
        out(1:n) = f(z(1:n))
      end associate
    end do
    print *, out(1), out(2)
    if (abs(out(1) - 2.0) > 1e-6) error stop
    if (abs(out(2) - 2.0) > 1e-6) error stop
  end subroutine
end module
program gpu_metal_177
  use gpu_metal_177_m
  type(data_t) :: arr(2)
  arr(1) = data_t([1.0, 2.0])
  arr(2) = data_t([3.0, 4.0])
  call run(arr, 2)
end program
