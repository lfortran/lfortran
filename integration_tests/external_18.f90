program external_18
    use external_18_mod
    implicit none
    ! Re-declare the (private, hence not use-associated) external function.
    ! In the original netcdf-fortran failure this declaration came from an
    ! F77 `include` file; an inline re-declaration exercises the identical
    ! semantic code path (a bogus "already declared in the same scope" error).
    character(len=80), external :: get_libvers
    character(len=80) :: v
    if (nc_flag /= 42) error stop "public use-associated constant is wrong"
    v = get_libvers()
    print *, trim(v)
    if (trim(v) /= "netcdf-fortran 1.2.3") error stop "wrong version string"
end program external_18
