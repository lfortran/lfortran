program openmp_18

!*****************************************************************************80
!
!! MAIN is the main program for COMPUTE_PI.
!
!  Discussion:
!
!    COMPUTE_PI estimates the value of PI.
!
!    This program uses Open MP parallelization directives.  
!
!    It should run properly whether parallelization is used or not.
!
!    However, the parallel version computes the sum in a different
!    order than the serial version; some of the quantities added are
!    quite small, and so this will affect the accuracy of the results.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 April 2009
!
!  Author:
!
!    John Burkardt
!
  use omp_lib

  implicit none

  integer, parameter :: logn_max = 9 ! change to 10 for more accurate results

  print *, '\n'
  print *, 'COMPUTE_PI'
  print *, '  FORTRAN90/OpenMP version'
  print *, '\n'
  print *, '  Estimate the value of PI by summing a series.'
  call omp_set_num_threads(8)

  print *, '\n'
  print *, '  The number of processors available = ', omp_get_num_procs ( )
  print *, '  The number of threads available    = ', omp_get_max_threads ( )


  call r8_test ( logn_max )
!
!  Terminate.
!
  print *, '\n'
  print *, 'COMPUTE_PI'
  print *, '  Normal end of execution.'

  stop 0
end
subroutine r8_test ( logn_max )

!*****************************************************************************80
!
!! R8_TEST estimates the value of PI using double precision.
!
!  Discussion:
!
!    PI is estimated using N terms.  N is increased from 10^2 to 10^LOGN_MAX.
!    The calculation is repeated using both sequential and Open MP enabled code.
!    Wall clock time is measured by calling SYSTEM_CLOCK.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    18 April 2009
!
!  Author:
!
!    John Burkardt
!
  use omp_lib

  implicit none

  double precision error
  double precision estimate
  integer logn
  integer logn_max
  character ( len = 3 ) mode
  integer n
  double precision, parameter :: r8_pi = 3.141592653589793D+00
  double precision wtime

  print *, ' '
  print *, 'R8_TEST:'
  print *, '  Estimate the value of PI,'
  print *, '  using double precision arithmetic.'
  print *, ' '
  print *, '  N = number of terms computed and added;'
  print *, ' '
  print *, '  MODE = SEQ for sequential code;'
  print *, '  MODE = OMP for Open MP enabled code;'
  print *, '  (performance depends on whether Open MP is used,'
  print *, '  and how many processes are available!)'
  print *, ' '
  print *, '  ESTIMATE = the computed estimate of PI;'
  print *, ' '
  print *, '  ERROR = ( the computed estimate - PI );'
  print *, ' '
  print *, '  TIME = elapsed wall clock time;'
  print *, ' '
  print *, '  Note that you can''t increase N forever, because:'
  print *, '  A) ROUNDOFF starts to be a problem, and'
  print *, '  B) maximum integer size is a problem.'
  print *, ' '
  print *, '  The maximum integer:' , huge ( n )
  print *, ' '
  print *, ' '
  print *, '             N Mode    Estimate        Error           Time'
  print *, ' '

  n = 1

  do logn = 1, logn_max
! 
!  Sequential calculation.
!
    mode = 'SEQ'

    wtime = omp_get_wtime ( )

    call r8_pi_est_seq ( n, estimate )

    wtime = omp_get_wtime ( ) - wtime

    error = abs ( estimate - r8_pi )
    if (logn == logn_max .and. error > 1e-12) error stop

    print *, &
      n, mode, estimate, error, wtime
!
!  Open MP enabled calculation.
!
    mode = 'OMP'

    wtime = omp_get_wtime ( )

    call r8_pi_est_omp ( n, estimate )

    wtime = omp_get_wtime ( ) - wtime

    error = abs ( estimate - r8_pi )

    print *, &
      n, mode, estimate, error, wtime

    n = n * 10

  end do

  return
end
subroutine r8_pi_est_omp ( n, estimate )

!*****************************************************************************80
!
!! R8_PI_EST_OMP estimates the value of PI, using Open MP.
!
!  Discussion:
!
!    The calculation is based on the formula for the indefinite integral:
!
!      Integral 1 / ( 1 + X**2 ) dx = Arctan ( X ) 
!
!    Hence, the definite integral
!
!      Integral ( 0 <= X <= 1 ) 1 / ( 1 + X**2 ) dx 
!      = Arctan ( 1 ) - Arctan ( 0 )
!      = PI / 4.
!
!    A standard way to approximate an integral uses the midpoint rule.
!    If we create N equally spaced intervals of width 1/N, then the
!    midpoint of the I-th interval is 
!
!      X(I) = (2*I-1)/(2*N).  
!
!    The approximation for the integral is then:
!
!      Sum ( 1 <= I <= N ) (1/N) * 1 / ( 1 + X(I)**2 )
!
!    In order to compute PI, we multiply this by 4; we also can pull out
!    the factor of 1/N, so that the formula you see in the program looks like:
!
!      ( 4 / N ) * Sum ( 1 <= I <= N ) 1 / ( 1 + X(I)**2 )
!
!    Until roundoff becomes an issue, greater accuracy can be achieved by 
!    increasing the value of N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    13 January 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of terms to add up.
!
!    Output, double precision ESTIMATE, the estimated value of pi.
!
  implicit none

  double precision h
  double precision estimate
  integer i
  integer n
  double precision sum2
  double precision x

  h = 1.0D+00 / dble ( 2 * n )

  sum2 = 0.0D+00

!$omp parallel shared(h, n) private(i, x) reduction(+:sum2)
!$omp do
  do i = 1, n
    x = h * dble ( 2 * i - 1 )
    sum2 = sum2 + 1.0D+00 / ( 1.0D+00 + x**2 )
  end do
!$omp end do
!$omp end parallel

  estimate = 4.0D+00 * sum2 / dble ( n )

  return
end
subroutine r8_pi_est_seq ( n, estimate )

!*****************************************************************************80
!
!! R8_PI_EST_SEQ estimates the value of PI, using sequential execution.
!
!  Discussion:
!
!    The calculation is based on the formula for the indefinite integral:
!
!      Integral 1 / ( 1 + X**2 ) dx = Arctan ( X ) 
!
!    Hence, the definite integral
!
!      Integral ( 0 <= X <= 1 ) 1 / ( 1 + X**2 ) dx 
!      = Arctan ( 1 ) - Arctan ( 0 )
!      = PI / 4.
!
!    A standard way to approximate an integral uses the midpoint rule.
!    If we create N equally spaced intervals of width 1/N, then the
!    midpoint of the I-th interval is 
!
!      X(I) = (2*I-1)/(2*N).  
!
!    The approximation for the integral is then:
!
!      Sum ( 1 <= I <= N ) (1/N) * 1 / ( 1 + X(I)**2 )
!
!    In order to compute PI, we multiply this by 4; we also can pull out
!    the factor of 1/N, so that the formula you see in the program looks like:
!
!      ( 4 / N ) * Sum ( 1 <= I <= N ) 1 / ( 1 + X(I)**2 )
!
!    Until roundoff becomes an issue, greater accuracy can be achieved by 
!    increasing the value of N.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    06 January 2003
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of terms to add up.
!
!    Output, double precision ESTIMATE, the estimated value of pi.
!
  implicit none

  double precision h
  double precision estimate
  integer i
  integer n
  double precision sum2
  double precision x

  h = 1.0D+00 / dble ( 2 * n )

  sum2 = 0.0D+00

  do i = 1, n
    x = h * dble ( 2 * i - 1 )
    sum2 = sum2 + 1.0D+00 / ( 1.0D+00 + x**2 )
  end do

  estimate = 4.0D+00 * sum2 / dble ( n )

  return
end
