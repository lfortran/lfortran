program intrinsics_147
  integer :: m(5)
  integer :: res
  double precision :: n(5)
  double precision :: res_d
  real :: res_r
  real :: o(5)
  logical :: b
  logical :: a(5)
  real :: vector(7)
  complex :: p(2)

  vector = [5.0, 6.0, 3.0, 4.0, 0.0, 13.0, 29.0]
  m = [1, 0, 0, 2, 99]
  a = [.false., .false., .false., .false., .true.]

  print *, pack(m, (/.false., .false., .false., .false., .true./), (/ 5, 6, 3, 4, 0, 13, 29 /))
  res = sum(pack(m, (/.false., .false., .false., .false., .true./), (/ 5, 6, 3, 4, 0, 13, 29 /)))
  print *, res
  if (res /= 154) error stop

  b = .true.
  print *, pack(m, b, (/ 5, 6, 3, 4, 0, 13, 29 /))
  res = sum(pack(m, b, (/ 5, 6, 3, 4, 0, 13, 29 /)))
  print *, res
  if (res /= 144) error stop

  print *, pack(m, m /= 0, (/ 5, 6, 3, 4, 0, 13, 29 /))
  res = sum(pack(m, m /= 0, (/ 5, 6, 3, 4, 0, 13, 29 /)))
  print *, res
  if (res /= 148) error stop

  n = [1.0D0, 0.0D0, 0.0D0, 2.0D0, 99.0D0]

  print*, pack((/ 1.0D0, 0.0D0, 0.0D0, 2.0D0, 99.0D0 /), (/.false., .false., .false., .false., .true./), &
  (/ 5.0D0, 6.0D0, 3.0D0, 4.0D0, 0.0D0, 13.0D0, 29.0D0 /))
  res_d = sum(pack((/ 1.0D0, 0.0D0, 0.0D0, 2.0D0, 99.0D0 /), (/.false., .false., .false., .false., .true./), &
  (/ 5.0D0, 6.0D0, 3.0D0, 4.0D0, 0.0D0, 13.0D0, 29.0D0 /)))
  print *, res_d
  if (abs(res_d - 154.0D0) > 1e-12) error stop

  print *, res_d
  res_d = sum(pack(n, (/.false., .false., .false., .false., .true./), &
  (/ 5.0D0, 6.0D0, 3.0D0, 4.0D0, 0.0D0, 13.0D0, 29.0D0 /)))
  if (abs(res_d - 154.0D0) > 1e-12) error stop

  print *, pack((/ 1.0, 0.0, 0.0, 2.0, 99.0 /), a, &
  (/ 5.0, 6.0, 3.0, 4.0, 0.0, 13.0, 29.0 /))
  res_r = sum(pack((/ 1.0, 0.0, 0.0, 2.0, 99.0 /), (/.false., .false., .false., .false., .true./), &
  (/ 5.0, 6.0, 3.0, 4.0, 0.0, 13.0, 29.0 /)))
  print *, res_r
  if (abs(res_r - 154.0) > 1e-8) error stop

  o = [1.0, 0.0, 0.0, 2.0, 99.0]

  print *, pack(o, a, vector)
  res_r = sum(pack(o, a, vector))
  print *, res_r
  if (abs(res_r - 154.0) > 1e-6) error stop

  p = [(1.0, 2.0), (2.0, 3.0)]
  res_r = abs(sum(pack(p, .true., [(123.123, 9814.14), (-124151.41, 414.1)])))
  print *, res_r
  if (abs(res_r - 5.83095169) > 1e-8) error stop
end program
