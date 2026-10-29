module descriptor_entry
  implicit none
  type accumulator
    real :: total = 0
  contains
    procedure :: accumulate
    procedure :: observe
    procedure :: indirect_observe
    procedure :: optional_input
    procedure :: optional_first
    procedure :: saved_state
    procedure :: forward_input
    procedure :: nested_observer
    procedure :: declaration_observer
  end type
contains
  pure subroutine accumulate(self, x, y, lower)
    class(accumulator), intent(inout) :: self
    integer, intent(in) :: lower
    real, intent(in) :: x(lower:), y(-2:,:)
    real :: work(size(x))
    integer :: i, j
    work = x * 2
    do i = lower, lower + size(x) - 1
      self%total = self%total + work(i-lower+1)
    end do
    do j = 1, size(y, 2)
      do i = -2, ubound(y, 1)
        self%total = self%total + y(i,j)
      end do
    end do
  end subroutine

  subroutine observe(self, x)
    class(accumulator), intent(inout) :: self
    real, intent(in) :: x(:)
    if (is_contiguous(x)) then
      self%total = 1
    else
      self%total = -1
    end if
  end subroutine

  subroutine optional_input(self, x)
    class(accumulator), intent(inout) :: self
    real, optional, intent(in) :: x(:)
    self%total = 7
    if (present(x)) self%total = sum(x)
  end subroutine

  subroutine optional_first(self, x)
    class(accumulator), intent(inout) :: self
    real, optional, intent(in) :: x(:)
    self%total = 7
    if (present(x)) self%total = x(1)
  end subroutine

  logical function contiguous_input(x)
    real, target, intent(in) :: x(:)
    contiguous_input = is_contiguous(x)
  end function

  subroutine indirect_observe(self, x)
    class(accumulator), intent(inout) :: self
    real, intent(in) :: x(:)
    if (contiguous_input(x(:))) then
      self%total = 1
    else
      self%total = -1
    end if
  end subroutine

  subroutine saved_state(self, x)
    class(accumulator), intent(inout) :: self
    real, intent(in) :: x(:)
    integer, save :: count = 0
    count = count + 1
    self%total = count + sum(x)
  end subroutine

  pure subroutine forward_input(self, x)
    class(accumulator), intent(inout) :: self
    real, intent(in) :: x(-3:)
    call sum_input(self%total, x)
  end subroutine

  pure subroutine sum_input(total, x)
    real, intent(out) :: total
    real, intent(in) :: x(:)
    total = sum(x)
  end subroutine

  pure integer function selection_step(x)
    real, target, intent(in) :: x(:)
    selection_step = -1
    if (is_contiguous(x)) selection_step = 1
  end function

  subroutine nested_observer(self, x)
    class(accumulator), intent(inout) :: self
    real, intent(in) :: x(:)
    real :: work(size(x))
    work = x(size(x):1:selection_step(x))
    self%total = work(1)
  end subroutine

  pure integer function observer_size(x)
    real, target, intent(in) :: x(:)
    observer_size = 2
    if (is_contiguous(x)) observer_size = 1
  end function

  subroutine declaration_observer(self, x)
    class(accumulator), intent(inout) :: self
    real, intent(in) :: x(:)
    real :: work(observer_size(x))
    self%total = real(size(work))
  end subroutine
end module
