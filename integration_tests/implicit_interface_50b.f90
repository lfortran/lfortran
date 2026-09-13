module implicit_interface_50_mod
  character(len=80), external :: nf_inq_libvers
end module implicit_interface_50_mod

character(len=80) function nf_inq_libvers()
  nf_inq_libvers = "netcdf-fortran 1.2.3"
end function nf_inq_libvers
