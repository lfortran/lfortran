submodule(submodule_25_m) submodule_25_s
  implicit none
contains
  module procedure apply
    allocate(out(size(vec)))
    out = 2.0d0 * vec
  end procedure
  module procedure make_tensor
    r%n_ = n
    allocate(r%data_(size(data)))
    r%data_ = data
  end procedure
  module procedure compute
    ! Bug: keyword argument not found when using user-defined operator result
    ! directly as a positional argument to a generic interface call.
    t = tensor_t(op_t() .x. [1.0d0, 2.0d0], n=2)
  end procedure
end submodule
