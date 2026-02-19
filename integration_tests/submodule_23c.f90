submodule(submodule_23_mod) submodule_23_sub
  implicit none
contains
  module procedure check
    found = size(args) > 0
  end procedure
end submodule