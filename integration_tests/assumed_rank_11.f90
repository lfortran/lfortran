program main
  implicit none
  integer, target :: a(2)

  call take_pointer_rank(a)
  print *, "ok"
contains
  subroutine take_pointer_rank(x)
    integer, pointer, intent(in) :: x(..)

    ! Pass to a non-pointer helper to safely read the descriptor.
    ! This avoids an unrelated LFortran LLVM lowering bug for pointer dummies,
    ! while strictly verifying the descriptor passed by the caller.
    call check_rank(x)
  end subroutine take_pointer_rank

  subroutine check_rank(y)
    integer, intent(in) :: y(..)
    
    ! On main: y receives a raw data pointer, so rank reads garbage and fails.
    ! On this PR: y receives the valid descriptor, rank is 1, and it passes.
    if (rank(y) /= 1) error stop
  end subroutine check_rank
end program main
