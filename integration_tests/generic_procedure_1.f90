module custom_deque
  implicit none
  type :: Complex32Deque
    integer :: size_ = 0
  contains
    procedure :: Complex32Deque_resize_default
    generic :: resize => Complex32Deque_resize_default
  end type Complex32Deque

contains

  subroutine Complex32Deque_resize_default(this, count, value)
    class(Complex32Deque), intent(inout) :: this
    integer, intent(in) :: count
    integer, optional, intent(in) :: value

    ! Simplified implementation to avoid undefined dependencies
    if (count >= 0) then
      this%size_ = count
      if (present(value)) then
        ! Simulate setting value (not implemented)
      end if
    end if
  end subroutine Complex32Deque_resize_default

end module custom_deque

program generic_procedure_1
  use custom_deque
  implicit none

  type(Complex32Deque) :: d
  integer :: rc=0

  call d%resize(5, value=5)
  print *, "Size:", d%size_, "RC:", rc
end program generic_procedure_1