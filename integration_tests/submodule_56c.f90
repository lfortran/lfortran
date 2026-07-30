submodule(intrinsic_array_m_submodule_56) intrinsic_array_s_submodule_56
  implicit none
contains
  module procedure construct
    select rank(array)
    rank(1)
      select type(array)
      type is(integer)
        allocate(intrinsic_array%integer_1D, source = array)
      class default
        error stop "submodule_56: unsupported rank-1 type"
      end select
    rank default
      error stop "submodule_56: unsupported rank"
    end select
  end procedure

  module procedure as_character
    s = "ok"
  end procedure
end submodule intrinsic_array_s_submodule_56
