module intent_out_polymorphic_dyn_03_m
   implicit none
   type :: b
      integer, allocatable :: x(:)
   end type
   type, extends(b) :: e
      integer, allocatable :: y(:)
   end type
   integer :: n_present = 0
contains
   subroutine s(v)
      class(b), intent(out), optional :: v
      if (present(v)) then
         n_present = n_present + 1
         select type (v)
         type is (e)
            ! y was deallocated on entry, so it can be allocated again
            if (allocated(v%y)) error stop 10
            allocate (v%y(3))
         end select
      end if
   end subroutine
   ! An absent optional dummy of another type, forwarded to `s`
   subroutine fwd_t(v)
      type(e), intent(inout), optional :: v
      call s(v)
   end subroutine
   subroutine fwd_c(v)
      class(e), intent(inout), optional :: v
      call s(v)
   end subroutine
   subroutine fwd_b(v)
      type(b), intent(inout), optional :: v
      call s(v)
   end subroutine
   subroutine fwd_cb(v)
      class(b), intent(inout), optional :: v
      call s(v)
   end subroutine
end module

program intent_out_polymorphic_dyn_03
   use intent_out_polymorphic_dyn_03_m
   implicit none
   type(e) :: c
   type(b) :: d

   ! present
   allocate (c%x(1), c%y(1))
   call s(c)
   if (allocated(c%x)) error stop 1
   if (.not. allocated(c%y)) error stop 2
   if (size(c%y) /= 3) error stop 3

   ! absent, directly and forwarded from optional dummies of every kind
   call s()
   call fwd_t()
   call fwd_c()
   call fwd_b()
   call fwd_cb()
   if (n_present /= 1) error stop 4

   ! present, forwarded
   call fwd_t(c)
   if (size(c%y) /= 3) error stop 5
   deallocate (c%y)
   allocate (c%y(1))
   call fwd_c(c)
   if (size(c%y) /= 3) error stop 6
   allocate (c%x(2))
   call fwd_cb(c)
   if (allocated(c%x) .or. size(c%y) /= 3) error stop 7
   allocate (d%x(2))
   call fwd_b(d)
   if (allocated(d%x)) error stop 8
   if (n_present /= 5) error stop 9

   print *, "ok"
end program
