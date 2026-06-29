program select_rank_34
implicit none

real, allocatable :: out1d(:)
real, allocatable :: out2d(:,:)
real, allocatable :: out3d(:,:,:)

call extract(out1d)
if (.not. allocated(out1d)) error stop "rank-1 not allocated"
if (size(out1d) /= 6) error stop "rank-1 size"
if (any(abs(out1d - [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]) > 1.0e-6)) error stop "rank-1 values"

call extract(out2d)
if (.not. allocated(out2d)) error stop "rank-2 not allocated"
if (size(out2d, 1) /= 2 .or. size(out2d, 2) /= 3) error stop "rank-2 shape"
if (any(abs(reshape(out2d, [6]) - [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]) > 1.0e-6)) error stop "rank-2 values"

call extract(out3d)
if (.not. allocated(out3d)) error stop "rank-3 not allocated"
if (size(out3d, 1) /= 1 .or. size(out3d, 2) /= 2 .or. size(out3d, 3) /= 3) error stop "rank-3 shape"
if (any(abs(reshape(out3d, [6]) - [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]) > 1.0e-6)) error stop "rank-3 values"

contains

subroutine extract(output)
   real, dimension(..), allocatable, intent(out) :: output
   select rank(output)
   rank(1)
      output = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [6])
   rank(2)
      output = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2, 3])
   rank(3)
      output = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [1, 2, 3])
   end select
end subroutine extract

end program select_rank_34