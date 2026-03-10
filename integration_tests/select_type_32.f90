module select_type_32_mod
  character(:), pointer, private :: string => null()
contains
  function get()
    class(*), pointer :: get
    if (.not.associated(string)) allocate(string, source='foo')
    get => string
  end function
  subroutine set(arg)
    class(*), intent(in) :: arg
    select type (arg)
    type is (character(*))
      if (arg /= 'foo') error stop
    end select
  end subroutine
end module

program select_type_32
  use select_type_32_mod
  logical :: entered
  entered = .false.
  select type (s => get())
  type is (character(*))
    entered = .true.
    call set(s)
  end select
  if (.not. entered) error stop
end program
