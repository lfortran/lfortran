program implied_do_loops48
! Compile time evaluation of intrinsics inside an implied do loop used to look
! the intrinsic up by its lowercased ASR enum name ("stringlentrim") instead of
! its id, which threw for every intrinsic whose enum name differs from its
! Fortran name. See https://github.com/lfortran/lfortran/issues/12666
implicit none
integer :: i

! the reproducer from the issue: len_trim is `StringLenTrim` in the ASR
integer, parameter :: opl(3) = [(len_trim('123456'), i = 1, 3)]

! further intrinsics whose ASR enum name is not their Fortran name
integer, parameter :: idx(2) = [(index('hello', 'll'), i = 1, 2)]
integer, parameter :: scn(2) = [(scan('hello', 'l'), i = 1, 2)]
integer, parameter :: vfy(2) = [(verify('hello', 'helo'), i = 1, 2)]
integer, parameter :: sik(2) = [(selected_int_kind(4), i = 1, 2)]
real, parameter :: lgm(2) = [(log_gamma(4.0), i = 1, 2)]

! an intrinsic whose enum name already matched, as a control
integer, parameter :: abv(3) = [(abs(-6), i = 1, 3)]

! the loop variable must still work alongside the intrinsic
integer, parameter :: mix(3) = [(len_trim('123456') + i, i = 1, 3)]

print *, opl
print *, idx, scn, vfy, sik
print *, lgm
print *, abv
print *, mix

if (any(opl /= 6)) error stop "len_trim in implied do failed"
if (any(idx /= 3)) error stop "index in implied do failed"
if (any(scn /= 3)) error stop "scan in implied do failed"
if (any(vfy /= 0)) error stop "verify in implied do failed"
if (any(sik /= 2)) error stop "selected_int_kind in implied do failed"
if (any(abs(lgm - 1.79175949) > 1e-6)) error stop "log_gamma in implied do failed"
if (any(abv /= 6)) error stop "abs in implied do failed"
if (any(mix /= [7, 8, 9])) error stop "len_trim + loop index in implied do failed"

end program implied_do_loops48
