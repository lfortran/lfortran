module dtsn01_small_m
  use dtsn01_base_m
  implicit none
  private
  type, extends(dataset_record) :: mesh_dataset     ! different layout
    integer(8) :: last_entity_count = 0
  end type
  type :: list
    type(mesh_dataset), allocatable :: vars(:)
  end type
  public :: run_small
contains
  subroutine run_small()
    type(list) :: l
    type(mesh_dataset) :: var
    var%internal_name = 's1'
    allocate(l%vars(1), source=var)
    var%internal_name = 's2'
    l%vars = [l%vars, var]
    var%internal_name = 's3'
    l%vars = [l%vars, var]
    call check(l%vars, 's')
  end subroutine
end module
