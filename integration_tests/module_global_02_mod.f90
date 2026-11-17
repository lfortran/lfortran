module module_global_02_m
  implicit none
  character(len=8) :: msg = "init"
  character(len=4) :: tags(2) = ["aaaa", "bbbb"]
  integer, target :: backing = 5
  integer, pointer :: alias => backing
end module module_global_02_m

subroutine module_global_02_set()
  use module_global_02_m, only: msg, tags, alias
  implicit none
  msg = "changed"
  tags(2) = "zzzz"
  alias = 11
end subroutine module_global_02_set
