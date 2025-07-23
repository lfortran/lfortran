module class_60_mod
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

subroutine call_describe(obj)
    class(base_t), intent(in) :: obj(:)
    class(base_t), allocatable :: obj_tmp
    allocate(obj_tmp)
    obj_tmp = obj(1)
    select type (obj_tmp)
      type is (extended_t)
        ! if (obj_tmp%key /= 10) error stop   ! TODO: Handle array arg in convert_to_polymorphic in LLVM Backend 
      class default
        error stop
    end select
  end subroutine

end module class_60_mod

program class_60
  use class_60_mod

  interface describe
    module procedure :: call_describe
  end interface

  type(temp_t) :: x
  x%child(1) = extended_t(10)
  x%child(2) = extended_t(20)
  call describe(x%child)

end program class_60