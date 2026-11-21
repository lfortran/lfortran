module compiler_version_04_mod
    use, intrinsic :: iso_fortran_env, only: compiler_version
    implicit none
    character(len=*), parameter :: compile_with = compiler_version()
end module

program compiler_version_04
    ! Named constants initialized with compiler_version()
    use, intrinsic :: iso_fortran_env, only: compiler_version
    use compiler_version_04_mod, only: compile_with
    implicit none
    character(len=*), parameter :: cw = compiler_version()
    character(len=200), parameter :: cw200 = compiler_version()
    character(len=*), parameter :: ct = trim(compiler_version())
    character(len=*), parameter :: cc = 'abc' // compiler_version()
    character(len=3), parameter :: c3 = compiler_version()

    if (len(cw) /= len(compiler_version())) error stop
    if (len(cw) <= 0) error stop
    if (cw /= compiler_version()) error stop
    if (len(cw200) /= 200) error stop
    if (cw200 /= compiler_version()) error stop
    if (len(ct) /= len_trim(compiler_version())) error stop
    if (ct /= compiler_version()) error stop
    if (len(cc) /= 3 + len(compiler_version())) error stop
    if (cc(1:3) /= 'abc') error stop
    if (cc(4:) /= compiler_version()) error stop
    if (c3 /= cw(1:3)) error stop
    if (compile_with /= cw) error stop
    print *, len(cw) > 0
end program
