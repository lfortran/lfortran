program test_dshiftr
  integer, parameter:: ik1 = selected_int_kind(1)
  integer(ik1), parameter:: i0 = 0, i1 = 1
  integer(ik1):: expected = 64
  if (abs(dshiftr(i1, i0, 2) - expected) > 1e-6) error stop
end program test_dshiftr
