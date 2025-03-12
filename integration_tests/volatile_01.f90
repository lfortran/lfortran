module sj0m_volatile_01
   implicit none
   integer, parameter :: m90_sngl = kind(1.e0)
   contains
      function sj0_sngl(x) result(f)
         integer, parameter :: knd = m90_sngl
         real(knd) f
         real(knd), intent(in) :: x
         real(knd), volatile :: iota = epsilon(x) / 2
         f = iota / 6
      end function sj0_sngl
end module sj0m_volatile_01

program volatile_01
    use sj0m_volatile_01
    implicit none
    real(m90_sngl) :: x = 1.0
    real(m90_sngl) :: y
    y = sj0_sngl(x)
    print *, y
    if ( abs( y - 9.93410776E-09 ) > 1E-15 ) error stop
end program
