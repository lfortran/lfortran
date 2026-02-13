submodule(mod_a) mod_a_impl
  implicit none
contains
  module procedure foo
    res = 1
  end procedure
end submodule
