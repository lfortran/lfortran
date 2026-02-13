submodule(submodule_21a) submodule_21b
  implicit none
contains

  module procedure assign_char
    lhs%string_ = rhs
  end procedure

  module procedure do_stuff
    associate(n => len(x%string_))
      x = "hello"
    end associate
  end procedure

end submodule
