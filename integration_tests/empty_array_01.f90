program empty_array_01
  integer, parameter :: x(0,0) = 0
  integer :: y(0,0)
  y = matmul(x, transpose(x))
  if (size(x) /= 0) error stop
  if (size(y) /= 0) error stop
  if (size(x,1) /= 0) error stop
  if (size(x,2) /= 0) error stop
  print *, "ok"
end
