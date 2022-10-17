integer, parameter :: dp = kind(0.d0)
real(dp) function dfloat(i,j)
integer, intent(in) :: i,j
dfloat = i*j
end function