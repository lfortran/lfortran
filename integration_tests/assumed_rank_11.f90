program assumed_rank_11
  implicit none

  integer :: value

  call check_storage_size(value)
  print *, "ok"

contains

  subroutine check_storage_size(arg)
    integer, intent(in), dimension(..) :: arg
    integer :: sample

    if (storage_size(arg) /= storage_size(sample)) error stop
  end subroutine check_storage_size

end program assumed_rank_11
