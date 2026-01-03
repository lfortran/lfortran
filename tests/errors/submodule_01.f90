module submodule_01_m
  implicit none

  interface submodule_01_m_i
  end interface

end module submodule_01_m

submodule(submodule_01_m) submodule_01_s
  implicit none

contains

  module procedure func
  end procedure

end submodule submodule_01_s

program submodule_01
    use submodule_01_m
    implicit none

end program submodule_01
