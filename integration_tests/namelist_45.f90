! A NAMELIST group in an internal subroutine whose object is not declared, so
! it is implicitly typed real (#12375).
subroutine namelist_45_host(res)
  real, intent(out) :: res
  call inner()
contains
    subroutine inner
      namelist /g3/ hc
      character(100) :: lines(5)
      hc = 2.5
      lines = ""
      write(lines, nml=g3)
      hc = 0
      read(lines, nml=g3)
      res = hc
    end subroutine
end subroutine

program namelist_45
  implicit none
  interface
    subroutine namelist_45_host(res)
      real, intent(out) :: res
    end subroutine
  end interface
  real :: r
  call namelist_45_host(r)
  print *, r
  if (abs(r - 2.5) > 1e-6) error stop
end program
