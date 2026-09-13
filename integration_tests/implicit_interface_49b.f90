module implicit_interface_49_mod
  integer, external :: nf_inq_format
end module implicit_interface_49_mod

integer function nf_inq_format()
  nf_inq_format = 7
end function nf_inq_format
