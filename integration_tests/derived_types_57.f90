module derived_types_57_m
   type :: ansi_code
      integer :: m = 44
   end type

    type :: term
        type(ansi_code) :: black = ansi_code()
    end type
end module

program derived_types_57
    use derived_types_57_m

    type(term) :: temp = term()

    if (temp%black%m /= 44) error stop
end program
