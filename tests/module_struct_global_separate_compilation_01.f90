module module_struct_global_separate_compilation_01
  implicit none
  type :: t
    integer :: x
  end type t
  type(t) :: targets
contains
  subroutine set_targets(v)
    integer, intent(in) :: v
    targets%x = v
  end subroutine set_targets
end module module_struct_global_separate_compilation_01

