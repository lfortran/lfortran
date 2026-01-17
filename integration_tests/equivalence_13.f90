program equivalence_13
  implicit none
  integer :: ia(5), ic
  logical :: la(2), lc
  equivalence (la(1),lc),  (ia(5), ic)

  ic = -42
  ia = [1, 2, 3, 4, 5]
  if (ia(5) /= 5) error stop "ia(5) should be 5"
  if (ic /= 5) error stop "ic should be 5 (equivalenced to ia(5))"

  lc = .false.
  la = .true.
  if (.not. la(1)) error stop "la(1) should be true"
  if (.not. lc) error stop "lc should be true (equivalenced to la(1))"

end program equivalence_13
