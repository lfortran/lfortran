module separate_compilation_35a
  use iso_fortran_env
  implicit none
contains
  function get_version() result(r)
    character(len=:), allocatable :: r
    r = compiler_version()
  end function
end module
