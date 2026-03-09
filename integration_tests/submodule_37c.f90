submodule(submodule_37_mod) submodule_37_sub
  implicit none
contains

  module procedure create
    t%val_ = s
  end procedure

  module procedure get_val
    v = self%val_
  end procedure

  module procedure get_int_array
    v = int(self%get_real_array(mold=[0.]))
  end procedure

  module procedure get_real_array
    allocate(v(3))
    read(self%val_, *) v
  end procedure

end submodule submodule_37_sub
