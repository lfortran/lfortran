module real128_cast_03_mod
    use, intrinsic :: iso_fortran_env, only: real64, real128
    implicit none
    ! PARAMETER statements convert their initializer to the declared kind
    ! like `::` initializers; the kind-16 results are written to this
    ! module file.
    real(real128) :: half
    parameter (half = 0.5)
    real(real128) :: quarter
    parameter (quarter = 0.25_real64)
    real(real128) :: two
    parameter (two = 2)
    real(real64) :: three_halves
    parameter (three_halves = 1.5_real128)
    integer :: n
    parameter (n = 2.5_real128)
    complex(real64) :: zh
    parameter (zh = 1.5_real128)
end module real128_cast_03_mod
