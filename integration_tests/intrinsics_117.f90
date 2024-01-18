program intrinsics_117
  print *, mod( a = 17.5, p = 5.5 )
  print *, mod( a = 17.5d0, p = 5.5 )
  print *, mod( a = 17.5, p = 5.5d0 )

  if ( .not. mod( a = 17, p = 3 ) == 2 ) error stop
end program intrinsics_117
