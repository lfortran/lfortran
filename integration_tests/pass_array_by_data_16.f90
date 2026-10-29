program descriptor_entry_test
  use descriptor_entry
  implicit none
  type(accumulator), target :: storage
  class(accumulator), pointer :: obj
  real, pointer :: absent(:)
  real :: a(-4:7), b(-3:4, -2:3), saved_a(-4:7), saved_b(-3:4,-2:3)
  real, parameter :: constant(3) = [1., 2., 3.]
  integer :: i, j
  do i = -4, 7
    a(i) = real(i + 5)
  end do
  do j = -2, 3
    do i = -3, 4
      b(i,j) = real(i + 4 + 8*(j+2))
    end do
  end do
  saved_a = a
  saved_b = b
  obj => storage
  call obj%accumulate(a(-3:7:2), b(-3:4:2,-2:3:2), -5)
  if (abs(obj%total - (2*sum(a(-3:7:2)) + sum(b(-3:4:2,-2:3:2)))) > 1.e-5) error stop 1
  if (any(a /= saved_a) .or. any(b /= saved_b)) error stop 2
  obj%total = 0
  call obj%accumulate(constant, b, 3)
  if (abs(obj%total - (12 + sum(b))) > 1.e-5) error stop 3
  call obj%observe(a(::2))
  if (obj%total /= -1) error stop 4
  call obj%observe(a)
  if (obj%total /= 1) error stop 5
  call obj%optional_input()
  if (obj%total /= 7) error stop 6
  call obj%indirect_observe(a(::2))
  if (obj%total /= -1) error stop 7
  call obj%saved_state(constant)
  if (obj%total /= 7) error stop 8
  call obj%saved_state(constant)
  if (obj%total /= 8) error stop 9
  call obj%forward_input(a(::2))
  if (obj%total /= sum(a(::2))) error stop 10
  obj%total = 0
  call obj%accumulate(a(7:-3:-2), b(4:-3:-2,3:-2:-2), -5)
  if (obj%total /= 2*sum(a(7:-3:-2)) + sum(b(4:-3:-2,3:-2:-2))) error stop 11
  obj%total = 0
  call obj%accumulate(a(1:0), b(:,1:0), -5)
  if (obj%total /= 0) error stop 12
  call obj%optional_first()
  if (obj%total /= 7) error stop 13
  absent => null()
  call obj%optional_first(absent)
  if (obj%total /= 7) error stop 14
  call obj%optional_first(constant)
  if (obj%total /= 1) error stop 15
  call obj%nested_observer(a(::2))
  if (obj%total /= a(6)) error stop 16
  call obj%declaration_observer(a(::2))
  if (obj%total /= 2) error stop 17
  call obj%declaration_observer(a)
  if (obj%total /= 1) error stop 18
end program
