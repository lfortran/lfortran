program format_84
  implicit none

  real(8), parameter :: x = 1.23456789012345d0
  real(8), dimension(9), parameter :: expected_ru = [ &
       2.0d0, 1.3d0, 1.24d0, 1.235d0, 1.2346d0, 1.23457d0, 1.234568d0, 1.2345679d0, 1.2345679d0 ]
  real(8), dimension(9), parameter :: expected_rd_rz = [ &
       1.0d0, 1.2d0, 1.23d0, 1.234d0, 1.2345d0, 1.23456d0, 1.234567d0, 1.2345678d0, 1.23456789d0 ]

  call check_mode('RU', expected_ru)
  call check_mode('RD', expected_rd_rz)
  call check_mode('RZ', expected_rd_rz)

contains

  subroutine check_mode(mode, expected)
    character(len=*), intent(in) :: mode
    real(8), intent(in) :: expected(9)
    real(8) :: got(9)
    character(len=80) :: line, fmt
    integer :: i

    do i = 1, 9
      write(fmt, '("(",a,",d0.",i0,")")') trim(mode), i
      write(line, fmt) x
      read(line, '(d80.30)') got(i)
    end do

    if ( any(got /= expected) ) error stop
    
  end subroutine check_mode

end program format_84
