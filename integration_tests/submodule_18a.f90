program submodule_18b
  use julienne_m_submodule_18b
  implicit none
  character(len=:), allocatable :: fmt_str
  integer :: i

  fmt_str = separated_values(i)
  if (fmt_str /= "(*(G25.20,:,','))") error stop
end program submodule_18b
