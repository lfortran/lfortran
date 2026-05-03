module finalization_05_mod
   implicit none
   type :: my_type
      integer :: id = -1
      integer, allocatable :: payload(:)
   contains
      final :: finalise
   end type my_type
contains
   subroutine finalise(this)
      type(my_type), intent(inout) :: this
      if (allocated(this%payload)) deallocate(this%payload)
   end subroutine finalise
end module finalization_05_mod

program finalization_05
   use finalization_05_mod
   implicit none
   type(my_type), allocatable :: from_arr(:), to_arr(:)

   allocate(from_arr(2))
   from_arr(1)%id = 10
   from_arr(2)%id = 20

   allocate(to_arr(3))
   to_arr(1)%id = 1
   to_arr(2)%id = 2
   to_arr(3)%id = 3

   call move_alloc(from_arr, to_arr)

   if (allocated(from_arr)) error stop "from_arr should be deallocated"
   if (.not. allocated(to_arr)) error stop "to_arr should be allocated"
   if (size(to_arr) /= 2) error stop "to_arr has wrong size"
   if (to_arr(1)%id /= 10) error stop "to_arr(1) wrong id"
   if (to_arr(2)%id /= 20) error stop "to_arr(2) wrong id"

   print *, "ok"
end program finalization_05
