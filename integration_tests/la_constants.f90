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

print *, "szero: ", szero
if ( abs( szero - 0.0_sp) > 1.0e-8) error stop "szero test failed"

print *, "shalf: ", shalf
if ( abs( shalf - 0.5_sp) > 1.0e-8) error stop "shalf test failed"

print *, "sone: ", sone
if ( abs( sone - 1.0_sp) > 1.0e-8) error stop "sone test failed"

print *, "stwo: ", stwo
if ( abs( stwo - 2.0_sp) > 1.0e-8) error stop "stwo test failed"

print *,"sthree: ", sthree
if( abs( sthree - 3.0_sp) > 1.0e-8) error stop "sthree test failed"

print *, "sfour: ", sfour
if ( abs( sfour - 4.0_sp) > 1.0e-8) error stop "sfour test failed"

print *, "seight: ", seight
if ( abs( seight - 8.0_sp) > 1.0e-8) error stop "seight test failed"

print *, "sten: ", sten
if ( abs( sten - 10.0_sp) > 1.0e-8) error stop "sten test failed"

print *, "czero: ", czero
if ( abs( real(czero) - 0.0_sp) > 1.0e-8  .or. abs( aimag(czero) - 0.0_sp) > 1.0e-8)  error stop "czero test failed"

print *, "chalf: ", chalf
if ( abs( real(chalf) - 0.5_sp) > 1.0e-8  .or. abs( aimag(czero) - 0.0_sp) > 1.0e-8) error stop "chalf test failed"

print *, "cone: ", cone
if ( abs( real(cone) - 1.0_sp) > 1.0e-8  .or. abs( aimag(czero) - 0.0_sp) > 1.0e-8) error stop "cone test failed"

print *, "sulp: ", sulp
if (abs (sulp - 1.19209290e-7) > 1.0e-8) error stop "sulp test failed"

print *, "seps: ", seps
if (abs (seps - 5.96046448e-8) > 1.0e-8) error stop "seps test failed"

print *, "ssafmin: ", ssafmin
if (abs (ssafmin - 1.17549435e-38) > 1.0e-8 ) error stop "ssafmin test failed"

print *, "ssafmax: ", ssafmax 
if (abs (ssafmax - 8.50705917302346159e+37) > 1.0) error stop "ssafmax test failed"

print *, "ssmlnum: ", ssmlnum
if (abs (ssmlnum - 9.86076132e-32) > 1.0e-8 ) error stop "ssmlnum test failed"

print *, "sbignum: ", sbignum
if (abs (sbignum - 1.01412048018258352e+31) > 1.0e-8) error stop "sbignum test failed"

print *, "srtmin: ", srtmin
if (abs (srtmin - 3.14018486e-16) > 1.0e-8) error stop "srtmin test failed"

print *, "srtmax: ", srtmax
if (abs (srtmax - 3.18452583626288650e+15)/srtmax > 2.0e-8) error stop "srtmax test failed"

print *, "stsml: ", stsml
if (abs (stsml - 1.08420217e-19) > 1.0e-8) error stop "stsml test failed"

print *, "stbig: ", stbig
if (abs (stbig - 4.50359962737049600e+15) > 1.0e-8) error stop "stbig test failed"

print *, "sssml: ", sssml
if (abs (sssml - 3.77789318629571617e+22) > 1.0e-8) error stop "sssml test failed"

print *, "ssbig: ", ssbig
if (abs (ssbig - 1.32348898e-23) > 1.0e-8) error stop "ssbig test failed"

print*, "dzero: ", dzero
if ( abs( dzero - 0.0_dp) > 1.0d-16) error stop "dzero test failed"

print*, "dhalf: ", dhalf
if ( abs( dhalf - 0.5_dp) > 1.0d-16) error stop "dhalf test failed"

print*, "done: ", done
if ( abs( done - 1.0_dp) > 1.0d-16) error stop "done test failed"

print*, "dtwo: ", dtwo
if ( abs( dtwo - 2.0_dp) > 1.0d-16) error stop "dtwo test failed"

print*, "dthree: ", dthree
if ( abs( dthree - 3.0_dp) > 1.0d-16) error stop "dthree test failed"

print*, "dfour: ", dfour
if ( abs( dfour - 4.0_dp) > 1.0d-16) error stop "dfour test failed"

print*, "deight: ", deight
if ( abs( deight - 8.0_dp) > 1.0d-16) error stop "deight test failed"

print*, "dten: ", dten
if ( abs( dten - 10.0_dp) > 1.0d-16) error stop "dten test failed"

print *, "zzero: ", zzero 
if (abs(real(zzero) - 0.0_dp) > 1.0d-16 .or. abs(aimag(zzero) - 0.0_dp) > 1.0d-16) error stop "zzero test failed"

print *, "zhalf :", zhalf
if (abs(real(zhalf) - 0.5_dp) > 1.0d-16 .or. abs(aimag(zhalf) - 0.0_dp) > 1.0d-16) error stop "zhalf test failed"

print *, "zone: ", zone
if (abs(real(zone) - 1.0_dp) > 1.0d-16 .or. abs(aimag(zone) - 0.0_dp) > 1.0d-16) error stop "zone test failed"

print *, "dulp: ", dulp
if ( abs(dulp - 2.22044604925031308d-16) > 1.0d-16 ) error stop "dulp test failed"

print *, "deps: ", deps
if ( abs(deps - 1.11022302462515654d-16) > 1.0d-16 ) error stop "deps test failed"

print *, "dsafmin: ", dsafmin
if ( abs(dsafmin - 2.22507385850720138d-308) > 1.0d-16 ) error stop "dsafmin test failed"

print *, "dsafmax: ", dsafmax
if ( abs(dsafmax - 4.49423283715578977d+307) > 1.0d-16 ) error stop "dsafmax test failed"

print *, "dsmlnum: ", dsmlnum
if ( abs(dsmlnum - 1.00208418000448639d-292) > 1.0d-16 ) error stop "dsmlnum test failed"

print *, "dbignum: ", dbignum
if ( abs(dbignum - 9.97920154767359906d+291) > 1.0d-16 ) error stop "dbignum test failed"

print *, "drtmin: ", drtmin
if ( abs(drtmin - 1.00104154759155046d-146) > 1.0d-16 ) error stop "drtmin test failed"

print *, "drtmax: ", drtmax
if ( abs(drtmax - 9.98959536101117514d+145) > 1.0d-16 ) error stop "drtmax test failed"

print *, "dtsml: ", dtsml
if ( abs(dtsml - 1.49166814624004135d-154) > 1.0d-16 ) error stop "dtsml test failed"

print *, "dtbig: ", dtbig
if ( abs(dtbig - 1.99791907220223503d+146) > 1.0d-16 ) error stop "dtbig test failed"

print *, "dssml: ", dssml
if ( abs(dssml - 4.49891379454319638d+161) > 1.0d-16 ) error stop "dssml test failed"

print *, "dsbig: ", dsbig
if (abs ( dsbig - 1.1137937474253874d-163) > 1.0d-16) error stop "dsbig test failed"

print *, "All tests passed!"
end program
