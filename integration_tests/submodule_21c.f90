submodule(submodule_21b_string_mod) submodule_21c_string_sub
  implicit none
contains

  module procedure assign_character_to_string_t
    lhs%string_ = rhs
  end procedure

  module procedure file_extension
    extension = ""
  end procedure

end submodule submodule_21c_string_sub
