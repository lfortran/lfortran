module julienne_formats_m_submodule_18b
  implicit none

  interface

    pure module function separated_values(mold) result(format_string)
      class(*), intent(in) :: mold(..)
      character(len=:), allocatable :: format_string
    end function

  end interface

end module julienne_formats_m_submodule_18b


module julienne_m_submodule_18b
  use julienne_formats_m_submodule_18b
  implicit none
end module julienne_m_submodule_18b
