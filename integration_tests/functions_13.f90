module test_scan_module
implicit none

contains

subroutine test_scan_intrinsic()
    if( scan("fortran", "ao") /= 2 ) error stop          ! 2, found 'o'
    if( scan("fortran", "ao", .true.) /= 6 ) error stop ! 6, found 'a'
    if( scan("fortran", "c++") /= 0 ) error stop        ! 0, found none
end subroutine

end module

program test_scan_verify
use test_scan_module, only: test_scan_intrinsic
implicit none

call test_scan_intrinsic()
print *, test_verify_intrinsic()

contains

logical function test_verify_intrinsic() result(r)
    if( verify("fortran", "ao") /= 1) error stop           ! 1, found 'f'
    if( verify("fortran", "foo") /= 3) error stop          ! 3, found 'r'
    if( verify("fortran", "c++") /= 1) error stop         ! 1, found 'f'
    if( verify("fortran", "c++", .true.) /= 7 ) error stop  ! 7, found 'n'
    if( verify("fortran", "fortran") /= 0) error stop     ! 0' found none
    r = .true.
end function

end program
