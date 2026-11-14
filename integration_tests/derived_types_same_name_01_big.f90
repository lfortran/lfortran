module dtsn01_big_m
  use dtsn01_base_m
  implicit none
  private
  type, extends(dataset_record) :: mesh_dataset     ! different layout
    integer(8) :: dataset_id = -1, type_id = -1
    logical :: is_vector = .false.
  end type
  type :: list
    type(mesh_dataset), allocatable :: vars(:)
  end type
  public :: run_big
contains
  subroutine run_big()
    type(list) :: l
    type(mesh_dataset) :: var
    var%internal_name = 'b1'
    allocate(l%vars(1), source=var)
    var%internal_name = 'b2'
    l%vars = [l%vars, var]
    var%internal_name = 'b3'
    l%vars = [l%vars, var]
    call check(l%vars, 'b')
  end subroutine
end module
