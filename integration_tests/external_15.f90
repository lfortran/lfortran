! Test external character function shadowing intrinsic sin
! Related to issue #6702
implicit none
character(8), external:: sin
if (sin() /= 'Peccavi!') error stop
end program

character(8) function sin()
  sin = 'Peccavi!'
end function sin
