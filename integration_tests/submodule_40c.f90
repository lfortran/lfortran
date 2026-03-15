submodule(submodule_40_mod) submodule_40_submod
contains
  module procedure show_name
    if (names(1) /= "a") error stop 1
  end procedure show_name
end submodule submodule_40_submod
