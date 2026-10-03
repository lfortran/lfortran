module legacy_array_sections_16_mod
  public :: ddot

  contains
    real function ddot(n, dx, incx, dy, incy)
        integer :: incx, incy, n
        real :: dx(*), dy(*)
        real :: dtemp
        integer :: i, ix, iy, m, mp1
        dtemp = 0.0
        do i = 1, n
          dtemp = dtemp + dx(i) * dy(i)
        end do
        ddot = dtemp
    end function ddot
end module legacy_array_sections_16_mod

program legacy_array_sections_16
  use legacy_array_sections_16_mod
  implicit none

  contains

    real function dcv(nord, w)
        integer, intent(in) :: nord
        real :: w(*)

        real :: v(40)
        integer :: i, ipp, mdw

        v = 0.0
        ipp = 1
        mdw = 1
        do i = 1, nord
          v(i) = ddot(nord, w(ipp), 1, v(nord+1), 1)
          ipp = ipp + mdw
        end do
        dcv = v(1)
    end function dcv
end program
