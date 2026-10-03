! Test alternate returns in subroutines and entry statements
subroutine sub_alt(i, *, *)
  integer, intent(in) :: i
  if (i == 1) then
    return 1
  else if (i == 2) then
    return 2
  end if
end subroutine

subroutine sub_with_entry()
  integer :: i
  return
  entry ent_alt(i, *)
  return 1
end subroutine

program entry_15
  implicit none
  integer :: x

  ! Test 1: alternate return via first label
  x = 0
  call sub_alt(1, *10, *20)
  x = -1
  goto 30
10 x = 10
  goto 30
20 x = 20
30 continue
  if (x /= 10) error stop

  ! Test 2: alternate return via second label
  call sub_alt(2, *40, *50)
  x = -1
  goto 60
40 x = 40
  goto 60
50 x = 50
60 continue
  if (x /= 50) error stop

  ! Test 3: no alternate return (normal return)
  call sub_alt(3, *70, *80)
  x = 99
  goto 90
70 x = 70
  goto 90
80 x = 80
90 continue
  if (x /= 99) error stop

  ! Test 4: entry with alternate return
  x = 0
  call ent_alt(1, *100)
  x = -1
  goto 110
100 x = 100
110 continue
  if (x /= 100) error stop

  print *, "All tests passed!"
end program
