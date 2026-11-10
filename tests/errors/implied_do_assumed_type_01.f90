! An assumed-type dummy argument cannot be the variable of an implied DO
! (#12391).
subroutine s(i)
  type(*) :: i
  print *, [(i, i = 1,1)]  ! {Error} The implied do loop variable 'i' must be a scalar integer, not type(*)
end
