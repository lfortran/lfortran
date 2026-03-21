program implied_do_loops29
  use, intrinsic :: iso_fortran_env, only: int8, real32
  implicit none
  character(*), parameter :: s = '1.234'
  integer(int8), parameter :: d0 = ichar('0', int8), dot = ichar('.', int8) - d0
  integer :: e
  real(8), parameter :: base(76) = [ (10.0d0**(39-e), e=1,39), (10.0d0**(-e), e=1,37) ]
  integer(int8) :: i, p, pP, pE, resp
  integer :: v
  real(real32) :: x

  p = 1; pP = 127; v = 0
  do i = p, len(s)
    select case (iachar(s(i:i)) - d0)
    case (0:9)
      v = v*10 + (iachar(s(i:i)) - d0)
    case (dot)
      pP = i
    case default
      exit
    end select
  end do

  pE = len(s) + 1
  p = len(s) + 1 - pP
  resp = pE - min(pP, p)
  if (resp <= 0) resp = resp + 1
  print *, 'v = ', v
  print *, 'resp = ', resp
  print *, 'base(38 + resp) = ', base(42)
  x = real(v, real32) * real(base(38 + resp), real32)

  print '(a,1x,g0)', 'result :', x
  if (abs(x - 1.234) > 1.0e-6) error stop 1
end program
