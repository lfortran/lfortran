module dtsn01_base_m
  implicit none
  type, public :: named_record
    character(:), allocatable :: internal_name
  end type
  type, extends(named_record), public :: dataset_record
    logical :: is_temporal = .false.
    integer(8) :: offset = 0
  end type
contains
  subroutine check(items, p)
    class(named_record), intent(in) :: items(:)
    character(1), intent(in) :: p
    integer :: j
    if (size(items) /= 3) error stop
    do j = 1, 3
      if (.not. allocated(items(j)%internal_name)) error stop
      if (items(j)%internal_name /= p // achar(48 + j)) error stop
    end do
  end subroutine
end module
