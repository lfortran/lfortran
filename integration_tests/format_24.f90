program format_24
  implicit none
  complex(8) :: z
  real(8) :: rvals(3)
  character(len=*), parameter :: FMT = '(*(g0.5,1x))'
  integer, parameter :: WP = selected_real_kind(15,307)
  real(WP) :: g = 1
  rvals = [1.1d0, 2.2d0, 3.3d0]
  z = (1.23d0, -4.56d0)

  write(*,FMT) 'test: ', g
  print "(G0.17)", 1d0/3d0                       ! Real division with repeating decimal
  print "(G0.5)", 12345.6789d0                   ! Wide number, smaller precision
  print "(G0.17)", 0.00000001234567d0           ! Small magnitude number
  print "(G0.17)", 123456789012345.0d0          ! Large magnitude number
  print "(G0.17)", 1.234567890123456d0          ! High-precision number
  print "(G0.17)", -42.0d0                      ! Negative real
  print "(G0.17)", 0.0d0                        ! Zero
  print "(G0.17)", 42                           ! Integer
  print "(G0.17)", -1000                        ! Negative integer
  print "(G0.17)", .true.                       ! Logical true
  print "(G0.17)", .false.                      ! Logical false
  print "(G0.17)", "Hello G format!"            ! Character string
  print "(G0.17)", z                            ! Complex number
  print "(3G0.17)", rvals                       ! Array of real numbers
  print "(G0.17)", 123456789012345.d+4
  print "(G0.17)", -123456789012345.d+7
  print "(G0.7)", 0.00001234
  print "(G15.7)", 1.0
  print "(G15.7)", 1.23
  print "(G15.7)", 1234567.8
  print "(G15.7)", 0.0001234
  print "(G15.7)", 1234567.8
  print "(4G15.7)", 1.0, 1.23, 1234567.8, 0.0001234
end program format_24
