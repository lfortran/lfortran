module lfortran_intrinsic_iso_fortran_env
implicit none

! Kind type parameters for integers (F90/F2003)
integer, parameter :: int8 = 1
integer, parameter :: int16 = 2
integer, parameter :: int32 = 4
integer, parameter :: int64 = 8

! Kind type parameters for reals (F90/F2003/F2023)
integer, parameter :: real16 = -1     ! F2023: half-precision (not supported)
integer, parameter :: real32 = 4
integer, parameter :: real64 = 8
integer, parameter :: real128 = -1

! Kind type parameters for logicals (F2023)
integer, parameter :: logical8 = 1
integer, parameter :: logical16 = 2
integer, parameter :: logical32 = 4
integer, parameter :: logical64 = 8

! I/O unit numbers
integer, parameter :: input_unit = 5
integer, parameter :: output_unit = 6
integer, parameter :: error_unit = 0

! Kind arrays (F2008/F2018)
integer, parameter :: integer_kinds(4) = [1, 2, 4, 8]
integer, parameter :: real_kinds(2) = [4, 8]
integer, parameter :: character_kinds(1) = [1]
integer, parameter :: logical_kinds(4) = [1, 2, 4, 8]

! I/O status parameters (F2003/F2008)
integer, parameter :: iostat_end = -1
integer, parameter :: iostat_eor = -2                    ! F2003
integer, parameter :: iostat_inquire_internal_unit = -3   ! F2008

! Storage sizes in bits (F2003)
integer, parameter :: numeric_storage_size = 32
integer, parameter :: character_storage_size = 8
integer, parameter :: file_storage_size = 8

! Coarray stat constants (F2008/F2018)
integer, parameter :: stat_unlocked = 0
integer, parameter :: stat_locked = 1
integer, parameter :: stat_locked_other_image = 2
integer, parameter :: stat_stopped_image = 3
integer, parameter :: stat_failed_image = 4              ! F2018
integer, parameter :: stat_unlocked_failed_image = 5     ! F2018

! Atomic kinds (F2018)
integer, parameter :: atomic_int_kind = 4
integer, parameter :: atomic_logical_kind = 4

! Team constants (F2018)
integer, parameter :: initial_team = 0
integer, parameter :: current_team = 1
integer, parameter :: parent_team = 2

contains
function compiler_version() result(version)
    character(len=:), allocatable :: version
    version = _lfortran_compiler_version() ! note: LFortran takes this and creates an IntrinsicElementalFunction
end function compiler_version

function compiler_options() result(options)
    character(len=:), allocatable :: options
    ! During AST->ASR conversion, this function call is replaced with a CompilerOptions ASR node
    ! containing the compiler options string from lcompilers_commandline_options at compile time.
    ! This ensures the options are captured when this code is compiled, not when the main program runs.
end function compiler_options


end module
