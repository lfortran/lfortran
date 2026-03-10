module submodule_40_mod
  implicit none

  character(len=*), parameter :: names(*) = [character(len("a")) :: "a"]

  interface
    module subroutine show_name()
    end subroutine show_name
  end interface
end module submodule_40_mod
