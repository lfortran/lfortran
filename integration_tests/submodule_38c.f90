submodule(submodule_38_mod) submodule_38_child_sub
  implicit none
contains
  module procedure multiply
    r = self%k_ * vec
  end procedure
end submodule
