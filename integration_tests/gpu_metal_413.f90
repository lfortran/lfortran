program gpu_metal_413
  ! An array of derived type with derived-type components with allocatable
  ! components built on the device, as the FIATS training tests build their
  ! mini-batches: the elemental pair constructor makes an array of pairs on
  ! the device heap, the batch constructor copies it into the batch's
  ! allocatable component, and the batch is copied out into batches(it)
  ! after the launch, with every component at every depth.
  use gpu_metal_413_tensor_m, only : tensor_t
  use gpu_metal_413_pair_m, only : pair_t
  use gpu_metal_413_batch_m, only : batch_t
  implicit none
  integer, parameter :: nb = 2, ni = 5
  type(tensor_t), allocatable :: ins(:, :), outs(:, :)
  type(batch_t), allocatable :: batches(:)
  real :: harvest(3, nb, ni)
  integer :: b, it
  do it = 1, ni
    do b = 1, nb
      harvest(:, b, it) = [b, it, b * it] / 16.0
    end do
  end do
  allocate(ins(nb, ni), outs(nb, ni))
  do concurrent (b = 1:nb, it = 1:ni)
    ins(b, it) = tensor_t(harvest(:, b, it))
    outs(b, it) = total(ins(b, it))
  end do
  allocate(batches(ni))
  ! One batch is already there, with a pair of another number of elements.
  allocate(batches(3)%pairs_(4))
  do concurrent (it = 1:ni)
    batches(it) = batch_t(pair_t(ins(:, it), outs(:, it)))
  end do
  do it = 1, ni
    if (size(batches(it)%pairs_) /= nb) error stop 1
    do b = 1, nb
      associate (p => batches(it)%pairs_(b))
        if (size(p%inputs_%values_) /= 3) error stop 2
        if (any(p%inputs_%values_ /= harvest(:, b, it))) error stop 3
        if (size(p%outputs_%values_) /= 1) error stop 4
        if (abs(p%outputs_%values_(1) - sum(harvest(:, b, it))) > 1e-6) &
          error stop 5
      end associate
    end do
  end do
  print *, "ok"
contains
  pure function total(t) result(r)
    type(tensor_t), intent(in) :: t
    type(tensor_t) r
    r = tensor_t([sum(t%values_)])
  end function
end program
