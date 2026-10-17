module gpu_metal_448_m
  ! An array component reached through an associate name of a construct
  ! the loop is in, and through the derived-type object itself in the
  ! same loop. Inside an `associate` construct of the body, the overlapping
  ! assignment through the name is given a temporary in a scope of its own.
  ! The kernel is handed the component once: what one iteration writes
  ! through one name it reads through the other, and the host sees it.
  implicit none
  type :: net_t
    real, allocatable :: w(:,:)
  contains
    procedure :: half_rows
  end type
contains
  pure integer function half_rows(self)
    class(net_t), intent(in) :: self
    half_rows = size(self%w, 1) / 2
  end function

  subroutine write_through_name(self, out)
    class(net_t), intent(inout) :: self
    real, intent(out) :: out(:)
    integer :: i
    associate(w => self%w)
      do concurrent (i = 1:size(w, 2))
        associate(m => self%half_rows())
          w(1:m, i) = w(m+1:2*m, i) + i
        end associate
        out(i) = sum(self%w(:, i))
      end do
    end associate
  end subroutine

  subroutine write_through_object(self, out)
    class(net_t), intent(inout) :: self
    real, intent(out) :: out(:)
    integer :: i
    associate(w => self%w)
      do concurrent (i = 1:size(w, 2))
        associate(m => self%half_rows())
          self%w(m+1:2*m, i) = self%w(1:m, i) * 2
          out(i) = sum(w(m+1:2*m, i))
        end associate
      end do
    end associate
  end subroutine
end module

program gpu_metal_448
  use gpu_metal_448_m
  implicit none
  type(net_t) :: net
  real :: out(3)
  allocate(net%w(4,3))
  net%w(1:2,:) = 1
  net%w(3:4,:) = 5
  call write_through_name(net, out)
  if (any(out /= [22., 24., 26.])) error stop 1
  if (any(net%w(1,:) /= [6., 7., 8.])) error stop 2
  if (any(net%w(2,:) /= [6., 7., 8.])) error stop 3
  if (any(net%w(3:4,:) /= 5)) error stop 4
  call write_through_object(net, out)
  if (any(out /= [24., 28., 32.])) error stop 5
  if (any(net%w(3,:) /= [12., 14., 16.])) error stop 6
  if (any(net%w(4,:) /= [12., 14., 16.])) error stop 7
  if (any(net%w(1:2,:) /= net%w(3:4,:) / 2)) error stop 8
  print *, "ok"
end program
