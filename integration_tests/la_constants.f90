module la_constants_module
integer, parameter :: sp = kind(1.e0)
 
real(sp), parameter :: szero = 0.0_sp
 
real(sp), parameter :: shalf = 0.5_sp
 
real(sp), parameter :: sone = 1.0_sp
 
real(sp), parameter :: stwo = 2.0_sp
 
real(sp), parameter :: sthree = 3.0_sp
 
real(sp), parameter :: sfour = 4.0_sp
 
real(sp), parameter :: seight = 8.0_sp
 
real(sp), parameter :: sten = 10.0_sp
 
complex(sp), parameter :: czero = ( 0.0_sp, 0.0_sp )
 
complex(sp), parameter :: chalf = ( 0.5_sp, 0.0_sp )
 
complex(sp), parameter :: cone = ( 1.0_sp, 0.0_sp )
 
character *1, parameter :: sprefix = 'S'
 
character *1, parameter :: cprefix = 'C'
 
real(sp), parameter :: sulp = epsilon(0._sp)
 
real(sp), parameter :: seps = sulp * 0.5_sp
 
real(sp), parameter :: ssafmin = real(radix(0._sp), sp)**max( minexponent(0._sp)-1, 1-maxexponent(0._sp) )
 
real(sp), parameter :: ssafmax = sone / ssafmin
 
real(sp), parameter :: ssmlnum = ssafmin / sulp
 
real(sp), parameter :: sbignum = ssafmax * sulp
 
real(sp), parameter :: srtmin = sqrt(ssmlnum)
 
real(sp), parameter :: srtmax = sqrt(sbignum)
 
real(sp), parameter :: stsml = real(radix(0._sp), sp)**ceiling( (minexponent(0._sp) - 1) * 0.5_sp)
 
real(sp), parameter :: stbig = real(radix(0._sp), sp)**floor( (maxexponent(0._sp) - digits(0._sp) + 1) * 0.5_sp)
 
real(sp), parameter :: sssml = real(radix(0._sp), sp)**( - floor( (minexponent(0._sp) - digits(0._sp)) * 0.5_sp))
 
real(sp), parameter :: ssbig = real(radix(0._sp), sp)**( - ceiling( (maxexponent(0._sp) + digits(0._sp) - 1) * 0.5_sp))
 
integer, parameter :: dp = kind(1.d0)
 
real(dp), parameter :: dzero = 0.0_dp
 
real(dp), parameter :: dhalf = 0.5_dp
 
real(dp), parameter :: done = 1.0_dp
 
real(dp), parameter :: dtwo = 2.0_dp
 
real(dp), parameter :: dthree = 3.0_dp
 
real(dp), parameter :: dfour = 4.0_dp
 
real(dp), parameter :: deight = 8.0_dp
 
real(dp), parameter :: dten = 10.0_dp
 
complex(dp), parameter :: zzero = ( 0.0_dp, 0.0_dp )
 
complex(dp), parameter :: zhalf = ( 0.5_dp, 0.0_dp )
 
complex(dp), parameter :: zone = ( 1.0_dp, 0.0_dp )
 
character *1, parameter :: dprefix = 'D'
 
character *1, parameter :: zprefix = 'Z'
 
real(dp), parameter :: dulp = epsilon(0._dp)
 
real(dp), parameter :: deps = dulp * 0.5_dp
 
real(dp), parameter :: dsafmin = real(radix(0._dp), dp)**max( minexponent(0._dp)-1, 1-maxexponent(0._dp) )
 
real(dp), parameter :: dsafmax = done / dsafmin
 
real(dp), parameter :: dsmlnum = dsafmin / dulp
 
real(dp), parameter :: dbignum = dsafmax * dulp
 
real(dp), parameter :: drtmin = sqrt(dsmlnum)
 
real(dp), parameter :: drtmax = sqrt(dbignum)
 
real(dp), parameter :: dtsml = real(radix(0._dp), dp)**ceiling( (minexponent(0._dp) - 1) * 0.5_dp)
 
real(dp), parameter :: dtbig = real(radix(0._dp), dp)**floor( (maxexponent(0._dp) - digits(0._dp) + 1) * 0.5_dp)
 
real(dp), parameter :: dssml = real(radix(0._dp), dp)**( - floor( (minexponent(0._dp) - digits(0._dp)) * 0.5_dp))
 
real(dp), parameter :: dsbig = real(radix(0._dp), dp)**( - ceiling( (maxexponent(0._dp) + digits(0._dp) - 1) * 0.5_dp))
end module

program la_constants
use la_constants_module

print *, "dulp: ", dulp
if ( abs(dulp - 2.22044604925031308d-16) > 1.0d-8 ) error stop "dulp test failed"

print *, "deps: ", deps
if ( abs(deps - 1.11022302462515654e-16) > 1.0d-8 ) error stop "deps test failed"

print *, "dsafmin: ", dsafmin
if ( abs(dsafmin - 2.22507385850720138e-308) > 1.0d-8 ) error stop "dsafmin test failed"

print *, "dsmlnum: ", dsmlnum
if ( abs(dsmlnum - 1.00208418000448639e-292) > 1.0d-8 ) error stop "dsmlnum test failed"

print *, "drtmin: ", drtmin
if ( abs(drtmin - 1.00104154759155046e-146) > 1.0d-8 ) error stop "drtmin test failed"

print *, "dtsml: ", dtsml
if ( abs(dtsml - 1.49166814624004135e-154) > 1.0d-8 ) error stop "dtsml test failed"

print *, "dsbig: ", dsbig
if ( abs( dsbig - 1.1113793747425387e-162) > 1.0d-8 ) error stop "dsbig test failed"

print *, "All tests passed!"
end program

