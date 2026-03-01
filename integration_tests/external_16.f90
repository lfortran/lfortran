! file testspecific1.f90 
module specificmod
  implicit none
  integer, parameter:: dp=kind(1d0)
contains

  subroutine root2sqdp(sqrt)
    real(dp),external::sqrt
    real(dp):: result
    result = sqrt(2.0_dp)**2
    if (abs(result - 2.0_dp) > 1.0e-14_dp) stop 1
    print "(A,F0.16)",'check sqrt(2.0_dp)**2 = ',result
  end subroutine root2sqdp
  
end module specificmod

program testspecific1
use specificmod
  implicit none
  intrinsic dsqrt
  call root2sqdp(dsqrt)
  print *, 'Test passed'
end program testspecific1