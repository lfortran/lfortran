module implicit_interface_47_mod
  integer, external :: nf_def_var_fill
end module implicit_interface_47_mod

integer function nf_def_var_fill(ncid, varid, no_fill, fill_value) result(status)
  integer, intent(in) :: ncid, varid, no_fill, fill_value
  status = ncid + varid + no_fill + fill_value
end function nf_def_var_fill
