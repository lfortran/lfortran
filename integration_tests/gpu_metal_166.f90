program gpu_metal_166
  implicit none
  integer :: n, pair
  real :: w(3,3), result_sum
  real :: expected(3)

  n = 3
  w = 1.0
  result_sum = 0.0
  expected = 3.0

  do concurrent (pair = 1:2)
    block
      real :: v(3)
      integer :: i
      v = 1.0
      associate(unused => n)
        v(1:n) = matmul(w(1:n,1:n), v(1:n))
      end associate
      do i = 1, 3
        if (abs(v(i) - expected(i)) > 1e-6) error stop
      end do
    end block
  end do

  print *, "ok"
end program
