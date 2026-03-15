program kinds_02
  implicit none
  integer, parameter :: kr1 = selected_real_kind(1), maxkr = 3, &
       skr2 = selected_real_kind(precision(1.0_kr1)+1), &
       kr2  = merge(skr2, kr1, skr2 > 0), &
       skr3 = selected_real_kind(precision(1.0_kr2)+1), &
       kr3  = merge(skr3, kr2, skr3 > 0), &
       allkr(maxkr+1) = [kr1, kr2, kr3, kr3], &
       nkr = minloc(abs(allkr(1:maxkr) - allkr(2:maxkr+1)), 1)
  integer :: n

  if (kr1 <= 0) error stop "kr1 must be a valid kind"
  if (kr2 < kr1) error stop "kr2 must be >= kr1"
  if (precision(1.0_kr1) < 1) error stop "precision(kr1) must be >= 1"
  if (nkr < 1 .or. nkr > maxkr) error stop "nkr out of range"
  if (nkr >= 2) then
    if (precision(1.0_kr2) <= precision(1.0_kr1)) error stop "kr2 should have higher precision"
  end if

  call realkinds(nkr)
contains
  subroutine realkinds(nkr)
    integer, intent(in) :: nkr
    integer :: n
    integer :: goodkr(nkr)
    goodkr = allkr(1:nkr)
    print "(A,I0)", 'precision(1.0_kr1) = ', precision(1.0_kr1)
    if (nkr >= 2) print "(A,I0)", 'precision(1.0_kr2) = ', precision(1.0_kr2)
    if (nkr >= 3) print "(A,I0)", 'precision(1.0_kr3) = ', precision(1.0_kr3)
    write(*, "(A,I0,A)", advance='no') &
        'Number of different real kinds = ', nkr, ', real kinds:'
    do n = 1, nkr
      write(*, "(1X,I0)", advance=merge('no ', 'yes', n < nkr)) allkr(n)
    end do
    print "(3A)", 'There ', merge('may be', 'are no', nkr >= maxkr), ' more real kinds'
  end subroutine realkinds
end program kinds_02
