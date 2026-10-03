module implicit_interface_46_mod
  integer, external :: nf_create
end module implicit_interface_46_mod

integer function nf_create(a, b, c)
  integer :: a, b, c
  nf_create = a + b + c
end function nf_create
