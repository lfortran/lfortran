! A sectioned actual argument to a spliced device callee is copied into a
! contiguous temporary of the spliced block first, and the host has to be
! able to size that temporary before it launches the kernel.
!
! When the section is written over a derived-type component chain reached
! through an ASSOCIATE -- `u(:,j)` for `u => g%tiles_(k)%values_` -- the
! `:` is spelled `lbound(g%tiles_(k)%values_, 1) : ubound(...)`, with a
! separate copy of the chain in each bound.  The pass hoists such a chain
! into a scalar temporary read once per kernel, and it compared two copies
! of one chain by pointer, so each copy got a temporary of its own: the
! extent came out as `ubound(t2%values_, d) - lbound(t1%values_, d) + 1`,
! naming two different temporaries, which the host could not fold back to
! a single dimension.  Every such loop was left on the host.
program gpu_metal_270
   use gpu_metal_270_m
   implicit none
   integer, parameter :: nx = 5, ny = 4, nc = 3
   type(grid_t) :: g
   real :: out_col(nx, ny), out_row(nx, ny), out_fix(nc, ny)
   real :: c(nc, ny)
   real :: expected
   integer :: i, j
   real, parameter :: w = 2.0

   allocate(g%tiles_(2))
   allocate(g%tiles_(1)%values_(nx, ny))
   allocate(g%tiles_(2)%values_(ny, nx))

   do j = 1, ny
      do i = 1, nx
         g%tiles_(1)%values_(i, j) = real(i + 10*j)
         g%tiles_(2)%values_(j, i) = real(100*i + j)
      end do
   end do
   do j = 1, ny
      do i = 1, nc
         c(i, j) = real(3*i + j)
      end do
   end do

   ! Contiguous column section of a component chain, run-time extent.
   out_col = 0
   associate (u => g%tiles_(1)%values_)
      do concurrent (j = 1:size(u, 2))
         out_col(:, j) = shifted(w, u(:, j))
      end do
   end associate
   do j = 1, ny
      print *, out_col(:, j)
      do i = 1, nx
         expected = w*g%tiles_(1)%values_(i, j)
         if (i < nx) expected = expected + g%tiles_(1)%values_(i + 1, j)
         if (abs(out_col(i, j) - expected) > 1.0e-4) error stop "column"
      end do
   end do

   ! Non-contiguous row section of a component chain, run-time extent.
   out_row = 0
   associate (u => g%tiles_(2)%values_)
      do concurrent (j = 1:size(u, 1))
         out_row(:, j) = shifted(w, u(j, :))
      end do
   end associate
   do j = 1, ny
      print *, out_row(:, j)
      do i = 1, nx
         expected = w*g%tiles_(2)%values_(j, i)
         if (i < nx) expected = expected + g%tiles_(2)%values_(j, i + 1)
         if (abs(out_row(i, j) - expected) > 1.0e-4) error stop "row"
      end do
   end do

   ! A constant-extent section as the fence.
   out_fix = 0
   do concurrent (j = 1:ny)
      out_fix(:, j) = shifted(w, c(1:nc, j))
   end do
   do j = 1, ny
      print *, out_fix(:, j)
      do i = 1, nc
         expected = w*c(i, j)
         if (i < nc) expected = expected + c(i + 1, j)
         if (abs(out_fix(i, j) - expected) > 1.0e-4) error stop "fixed"
      end do
   end do

end program gpu_metal_270
