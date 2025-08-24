program derived_types_78 ! by @RonShepard in Fortran Discourse apart from write stmt below
   implicit none
   type ll_t
      character(:), allocatable :: text      ! line of text.
      type(ll_t), allocatable   :: history   ! previous lines of text.
   end type ll_t

   type(ll_t), allocatable :: list

   call push( 'first line', list )
   call push( 'second line', list )
   call push( 'this is the last line', list )
   call printall( list )

contains

   subroutine push( newtext, list )
      ! push a new line of text into the linked list.
      character(*), intent(in)  :: newtext
      type(ll_t), intent(inout), allocatable :: list
      type(ll_t), allocatable :: work
      allocate( work )
      work%text = newtext
      print *, allocated(work%history)
      call move_alloc( from=list, to=work%history )
      call move_alloc( from=work, to=list )
      return
   end subroutine push

   recursive subroutine printall( list )
      type(ll_t), intent(in) :: list
      if( allocated(list%history) ) call printall( list%history )
      write(*,'(a)') '"'//list%text//'"' ! @harperjf added '"' and //
      return
   end subroutine printall

end program
