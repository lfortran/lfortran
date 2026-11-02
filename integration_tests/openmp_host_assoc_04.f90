! An elemental host is not recursive, but threads run its instances at the
! same time: each instance that passes its internal procedure on needs its
! own thunk, so with OpenMP the host gets a full pool.
module openmp_host_assoc_04_m
implicit none
contains
  pure real function apply(f, x)
    interface
      pure real function f(y)
        real, intent(in) :: y
      end function
    end interface
    real, intent(in) :: x
    integer :: k
    apply = 0
    do k = 1, 1000
      apply = apply + f(x) / 1000
    end do
  end function
  elemental real function host(x)
    real, intent(in) :: x
    host = apply(g, x)
  contains
    pure real function g(y)
      real, intent(in) :: y
      g = y * x
    end function
  end function
end module openmp_host_assoc_04_m
program openmp_host_assoc_04
  use openmp_host_assoc_04_m
  implicit none
  integer :: i
  real :: a(20000)
  !$omp parallel do num_threads(4)
  do i = 1, 20000
    a(i) = host(real(mod(i, 7)))
  end do
  !$omp end parallel do
  do i = 1, 20000
    if (abs(a(i) - real(mod(i,7))**2) > 1e-2) then
      print *, i, a(i)
      error stop 1
    end if
  end do
  print *, "ok"
end program openmp_host_assoc_04
