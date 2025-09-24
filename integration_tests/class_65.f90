module class_65_mod
  implicit none

  type :: base_t
  end type

  type, extends(base_t) :: extended_t
    integer :: key
  end type

  type :: temp_t
    type(extended_t) :: child(2)
  end type


contains 

subroutine call_describe(obj, check)
    class(base_t), intent(in) :: obj
    integer, intent(inout) :: check
    class(base_t), allocatable :: obj_tmp
    allocate(obj_tmp)
    obj_tmp = obj
    select type (obj_tmp)
      type is (base_t)
        check = check + 3
      type is (extended_t)
        check = check + 2
        if (obj_tmp%key /= 10) error stop
      class default
        error stop
    end select
  end subroutine

end module class_65_mod

program class_65
  use class_65_mod

  interface describe
    module procedure :: call_describe
  end interface

  type(temp_t) :: x
  type(base_t) :: y
  integer :: count
  count = 0
  x%child(1) = extended_t(10)
  x%child(2) = extended_t(20)
  call describe(x%child(1), count)
  if (count /= 2) error stop
  call describe(y, count)
  if (count /= 5) error stop
end program class_65