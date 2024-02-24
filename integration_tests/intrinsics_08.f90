program intrinsics_08
  real, parameter :: x=tiny(1.0)
  real (kind = 8) :: b
  real (kind = 8), parameter :: y=tiny(b)
  real, parameter :: z=tiny([1.0, 2.0])
  print*, x
  print*, y
  print*, z
end program
