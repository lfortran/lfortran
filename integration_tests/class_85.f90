module class_85_mod
    implicit none
    type :: string_t
        character(:), allocatable :: s
    end type string_t
  type :: string_list_t
      type(string_t), allocatable :: items(:)
      type(string_t), allocatable :: item
  end type string_list_t
contains
    subroutine set_list(list)
        type(string_t), allocatable, intent(in) :: list(:)
        if (allocated(list)) error stop "list allocated"
    end subroutine set_list
    subroutine set_list_scalar(list_mem)
        type(string_t), allocatable, intent(in) :: list_mem
        if (allocated(list_mem)) error stop "list allocated"
    end subroutine set_list_scalar
end module class_85_mod


program class_85
    use class_85_mod
    implicit none
    type(string_list_t) :: mylist
    call set_list(mylist%items)
    call set_list_scalar(mylist%item)
    if (allocated(mylist%items)) error stop "mylist%items allocated"
    if (allocated(mylist%item)) error stop "mylist%item allocated"
end program class_85