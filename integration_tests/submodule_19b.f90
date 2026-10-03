module string_m_submodule_19
  use iso_c_binding, only : c_bool, c_size_t
  implicit none

  type test_diagnosis_t
  end type

  type string_t
    character(len=:), allocatable :: s
  end type

  interface

    elemental module subroutine assign_character_to_string_t(lhs)
      class(string_t), intent(inout) :: lhs
    end subroutine
  end interface

end module string_m_submodule_19


module assert_m_submodule_19
  use string_m_submodule_19
end module assert_m_submodule_19


module m_submodule_19
  use assert_m_submodule_19
end module m_submodule_19
