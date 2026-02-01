program allocatable_dummy_descriptor_01
  implicit none

  type :: item_t
    integer :: s
  end type item_t

  type :: container_t
    type(item_t), allocatable :: list(:)
  end type container_t

  type(item_t), allocatable :: src(:)
  type(container_t), allocatable :: containers(:)

  allocate(src(1))
  src(1)%s = 123

  allocate(containers(1))
  call get_list(src, containers(1)%list)

  if (.not. allocated(containers(1)%list)) error stop
  if (size(containers(1)%list) /= 1) error stop
  if (containers(1)%list(1)%s /= 123) error stop

contains

  subroutine get_list(src, list)
    type(item_t), allocatable, intent(in) :: src(:)
    type(item_t), allocatable, intent(out) :: list(:)

    if (.not. allocated(src)) return

    allocate(list(size(src)))
    list = src
  end subroutine get_list

end program allocatable_dummy_descriptor_01
