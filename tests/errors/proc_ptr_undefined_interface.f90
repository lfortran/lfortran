program proc_ptr_undefined_interface
    implicit none
    procedure(nosuch), pointer :: q => null()
    print *, associated(q)
end program
