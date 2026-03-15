program implicit_typing_08
  implicit logical (l)

! Test that statement functions correctly access local variables
! with implicit typing from the enclosing scope.

  ist_add (i) = i + j
  rst_add (ri) = ri + rj
  lst_or (li) = li .or. lj
  ist_identity () = identity

  j = 40
  k = ist_add (2)
  if (k /= 42) error stop

  rj = -41.5
  rk = rst_add (-0.5)
  if (abs (rk + 42.0) > 0.01) error stop

  lj = .true.
  lk = lst_or (.false.)
  if (.not. lk) error stop

  identity = -42
  k = ist_identity ()
  if (k /= -42) error stop

end program
