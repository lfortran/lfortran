program implicit_interface_41
  integer, parameter :: lexpr = 2
  integer :: ad(30), expr(0:4*lexpr)
  integer :: p, v, q, rd, d
  integer :: distbr, addrof, differ
  integer :: i

  do i = 1, 30
    ad(i) = i
  end do

  do i = 0, 4 * lexpr
    expr(i) = i
  end do

  p = 1
  v = 2
  rd = 0

  q = distbr(ad(30), distbr(ad(30), addrof(p, expr, lexpr), &
       rd, expr, lexpr), addrof(v, expr, lexpr), expr, lexpr)
  d = differ()
  if (q /= 83) error stop
  if (d /= 30) error stop
end program

integer function distbr(oper, l, r, expr, lexpr)
  integer :: oper, l, r, lexpr
  integer :: expr(0:4*lexpr)
  distbr = oper + l + r + expr(0) + expr(lexpr*2) + lexpr
end function distbr

integer function addrof(v, expr, lexpr)
  integer :: v, lexpr
  integer :: expr(0:4*lexpr)
  addrof = v + expr(lexpr) + lexpr
end function addrof

integer function differ()
  integer, parameter :: lexpr = 2
  integer :: ad(4), expr(0:4*lexpr)
  integer :: distbr, addrof
  integer :: i

  do i = 1, 4
    ad(i) = i
  end do

  do i = 0, 4 * lexpr
    expr(i) = i
  end do

  differ = distbr(ad(4), distbr(ad(3), addrof(1, expr, lexpr), &
       0, expr, lexpr), addrof(2, expr, lexpr), expr, lexpr)
end function differ
