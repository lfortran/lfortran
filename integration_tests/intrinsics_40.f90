program intrinsics_40
  real(4) r4
  real(8) r8
  r4 = 1.521E0_4
  r8 = 3.251_8
  if (aint(r4) /= 1.0) error stop
  if (aint(r8) /= 3.0) error stop
  if (anint(r4) /= 2.0) error stop
  if (anint(r8) /= 3.0) error stop
  r8 = aint(r4, 8)
  if (aint(r4) /= 1.0) error stop
  r8 = anint(r4, 8)
  if (anint(r4) /= 2.0) error stop
end program intrinsics_40
