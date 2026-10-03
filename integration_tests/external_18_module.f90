module external_18_mod
    implicit none
    private
    integer, parameter, public :: nc_flag = 42
    ! A PRIVATE, use-associated external function (mirrors netcdf's
    ! `nf_inq_libvers`). Because it is private, `use external_18_mod` must
    ! NOT import this name, so a re-declaration in the caller is legal.
    character(len=80), external :: get_libvers
end module external_18_mod

function get_libvers() result(v)
    implicit none
    character(len=80) :: v
    v = "netcdf-fortran 1.2.3"
end function get_libvers
