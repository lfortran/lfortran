program format_121
  implicit none
  real :: x(8)
  integer :: i

  x = [(real(i), i = 1, 8)]

  ! redundant outer parentheses around a list with a nested repeated group
  call check('((1x,3(1x,f5.1)))', [character(len=24) :: &
      '    1.0   2.0   3.0', '    4.0   5.0   6.0', '    7.0   8.0'], 3)

  ! repeated outer group containing a nested repeated group
  call check('(2(1x,2(f5.1)))', [character(len=24) :: &
      '   1.0  2.0   3.0  4.0', '   5.0  6.0   7.0  8.0'], 2)

  ! leading descriptor followed by a group with a nested group
  call check_a('(a,(1x,2(f5.1)))', [character(len=24) :: &
      'v   1.0  2.0', '   3.0  4.0', '   5.0  6.0', '   7.0  8.0'], 4)

contains

  subroutine check(fmt, expected, n)
    character(len=*), intent(in) :: fmt
    character(len=*), intent(in) :: expected(:)
    integer, intent(in) :: n
    integer :: u, k, ios
    character(len=80) :: line
    open (newunit=u, status='scratch', action='readwrite')
    write (u, fmt) x
    rewind (u)
    do k = 1, n
      read (u, '(a)', iostat=ios) line
      if (ios /= 0) error stop 1
      if (trim(line) /= trim(expected(k))) error stop 2
    end do
    read (u, '(a)', iostat=ios) line
    if (ios == 0) error stop 3
    close (u)
  end subroutine

  subroutine check_a(fmt, expected, n)
    character(len=*), intent(in) :: fmt
    character(len=*), intent(in) :: expected(:)
    integer, intent(in) :: n
    integer :: u, k, ios
    character(len=80) :: line
    open (newunit=u, status='scratch', action='readwrite')
    write (u, fmt) 'v', x
    rewind (u)
    do k = 1, n
      read (u, '(a)', iostat=ios) line
      if (ios /= 0) error stop 4
      if (trim(line) /= trim(expected(k))) error stop 5
    end do
    read (u, '(a)', iostat=ios) line
    if (ios == 0) error stop 6
    close (u)
  end subroutine

end program
