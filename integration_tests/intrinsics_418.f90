program intrinsics_418
   implicit none

   real, allocatable :: output_metrics(:,:)

   call use_nonalloc_sum()

   allocate(output_metrics(3,4))
   output_metrics = 1.0

   block
      real, allocatable :: mean_metrics(:)
      mean_metrics = sum(output_metrics, 1) / size(output_metrics, 1)
      print *, mean_metrics
      if (any(mean_metrics /= 1.0)) error stop
   end block

contains

   subroutine use_nonalloc_sum()
      real :: a(3,4), b(4)
      a = 2.0
      b = sum(a, 1)
      print *, b(1)
   end subroutine use_nonalloc_sum

end program intrinsics_418
