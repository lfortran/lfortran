program module_global_02
  use module_global_02_m, only: msg, tags, backing, alias
  implicit none
  interface
    subroutine module_global_02_set()
    end subroutine module_global_02_set
  end interface
  if (.not. associated(alias, backing)) error stop 1
  call module_global_02_set()
  if (backing /= 11 .or. alias /= 11) error stop 2
  if (msg /= "changed") error stop 3
  if (tags(1) /= "aaaa") error stop 4
  if (tags(2) /= "zzzz") error stop 5
end program module_global_02
