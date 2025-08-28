module submodule_12_m
  implicit none

  interface submodule_12_m_i
    pure module function Func() result(format_string)
      character(len=:), allocatable :: format_string
    end function
  end interface

end module submodule_12_m

submodule(submodule_12_m) submodule_12_s
  implicit none

contains

  module procedure Func
  end procedure

end submodule submodule_12_s

program submodule_12
    use submodule_12_m
    implicit none

end program submodule_12
