module Module_08
  interface
    module subroutine hi()
    end subroutine
  end interface
end module Module_08

submodule(Module_08) Submodule_8
contains
  module subroutine hi()
    print *, "Hello world"
  end subroutine hi
end submodule Submodule_8

program main
  use Module_08
end program
