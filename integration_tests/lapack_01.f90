subroutine dpttrf(n, d, e, info)
integer :: n, info
double precision :: d( * ), e( * )
double precision :: zero
parameter( zero = 0.0d+0 )
integer :: i, i4
double precision :: ei
intrinsic :: mod
info = 0
if( n.eq.0 ) return
i4 = mod( n-1, 4 )
do 10 i = 1, i4
   if( d( i ).le.zero ) then
      info = i
      go to 30
   end if
   ei = e( i )
   e( i ) = ei / d( i )
   d( i+1 ) = d( i+1 ) - e( i )*ei
10 continue
do 20 i = i4 + 1, n - 4, 4
   if( d( i ).le.zero ) then
      info = i
      go to 30
   end if
   ei = e( i )
   e( i ) = ei / d( i )
   d( i+1 ) = d( i+1 ) - e( i )*ei
   if( d( i+1 ).le.zero ) then
      info = i + 1
      go to 30
   end if
   ei = e( i+1 )
   e( i+1 ) = ei / d( i+1 )
   d( i+2 ) = d( i+2 ) - e( i+1 )*ei
   if( d( i+2 ).le.zero ) then
      info = i + 2
      go to 30
   end if
   ei = e( i+2 )
   e( i+2 ) = ei / d( i+2 )
   d( i+3 ) = d( i+3 ) - e( i+2 )*ei

   if( d( i+3 ).le.zero ) then
      info = i + 3
      go to 30
   end if
   ei = e( i+3 )
   e( i+3 ) = ei / d( i+3 )
   d( i+4 ) = d( i+4 ) - e( i+3 )*ei
20 continue
if( d( n ).le.zero ) info = n

30 continue
return

end subroutine

program lapack_01
   integer :: n = 3, info
   double precision :: d(3), e(2)
   d = [4, 5, 6]
   e = [1, 2]
   call dpttrf(n, d, e, info)
   print *, "sum(d): ", sum(d)
   if ( abs(sum(d) - 13.907894736842106) > 1e-6 ) error stop
   print *, "sum(e): ", sum(e)
   if ( abs(sum(e) - 0.67105263157894735) > 1e-7 ) error stop
   print *, "info: ", info
   if ( info /= 0 ) error stop
end program