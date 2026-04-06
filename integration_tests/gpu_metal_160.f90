module gpu_metal_160_mod
  implicit none
  type :: t
    real, allocatable :: x(:)
  end type
contains
  subroutine compute(self, n, result_arr)
    type(t), intent(in) :: self
    integer, intent(in) :: n
    real, intent(out) :: result_arr(:)
    integer :: i
    associate(b => self%x)
      do concurrent (i = 1:n)
        block
          real :: z(3)
          integer :: j
          do j = 1, 3
            z(j) = -b(j)
          end do
          result_arr(i) = z(1) + z(2) + z(3)
        end block
      end do
    end associate
  end subroutine
end module

program gpu_metal_160
  use gpu_metal_160_mod
  implicit none
  type(t) :: obj
  real :: res(4)
  integer :: i

  allocate(obj%x(3))
  obj%x(1) = 1.0
  obj%x(2) = 2.0
  obj%x(3) = 3.0

  call compute(obj, 4, res)

  do i = 1, 4
    if (abs(res(i) - (-6.0)) > 1e-6) error stop
  end do
  print *, "ok"
end program
