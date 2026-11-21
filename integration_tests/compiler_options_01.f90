module compiler_options_01_mod
    use, intrinsic :: iso_fortran_env, only: compiler_options
    implicit none
    character(len=*), parameter :: compile_options = compiler_options()
end module

program compiler_options_01
    ! Named constants initialized with compiler_options()
    use, intrinsic :: iso_fortran_env, only: compiler_options
    use compiler_options_01_mod, only: compile_options
    implicit none
    character(len=*), parameter :: co = compiler_options()
    character(len=5), parameter :: co5 = compiler_options()
    character(len=*), parameter :: ct = trim(compiler_options())
    character(len=*), parameter :: cc = 'x' // compiler_options()
    character(len=*), parameter :: arr(2) = [compiler_options(), compiler_options()]
    character(len=*) :: cs
    parameter (cs = compiler_options())

    if (len(co) /= len(compiler_options())) error stop
    if (co /= compiler_options()) error stop
    if (len(co5) /= 5) error stop
    if (len(co) >= 5) then
        if (co5 /= co(1:5)) error stop
    else
        if (co5 /= co) error stop
    end if
    if (len(ct) /= len_trim(compiler_options())) error stop
    if (ct /= compiler_options()) error stop
    if (len(cc) /= 1 + len(co)) error stop
    if (cc(1:1) /= 'x') error stop
    if (cc(2:) /= co) error stop
    if (len(arr(2)) /= len(co)) error stop
    if (arr(1) /= co .or. arr(2) /= co) error stop
    if (cs /= co) error stop
    if (len(compile_options) /= len(co)) error stop
    print *, len(co) >= 0
end program
