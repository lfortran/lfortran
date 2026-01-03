module class_62_mod
  implicit none
contains

  subroutine handle_generic(generic)
    class(*), intent(inout) :: generic

    select type(generic)
    type is (integer)
      call get_args(generic)
    end select
  end subroutine handle_generic

  subroutine get_args(x)
    integer, intent(out) :: x
    x = 3
  end subroutine get_args

end module class_62_mod


program class_62
  use class_62_mod
  implicit none
  integer :: value
  call handle_generic(value)
  if (value /= 3) error stop
end program class_62