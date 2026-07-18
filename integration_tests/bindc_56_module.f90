module bindc_56_module
 ! The kind must be written in uppercase (C_CHAR) on purpose: the semantic
 ! check that assigns the CChar string physical type to a bind(C)
 ! character(kind=c_char) dummy used to be case sensitive, so an uppercase
 ! C_CHAR incorrectly produced a DescriptorString physical type. When this
 ! module is compiled separately and then used by a program that reads an
 ! array section into a character array, codegen tripped an assertion.
 interface
  subroutine nc_inq(name) bind(C)
   use iso_c_binding, only: C_CHAR
   character(kind=C_CHAR) :: name(*)
  end subroutine
 end interface
end module
