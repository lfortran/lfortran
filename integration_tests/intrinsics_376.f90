module stdlib_logger_intrinsics_376    
    implicit none
contains
subroutine add_log_unit(stat )
        integer, intent(out), optional    :: stat
        call validate_unit()
    contains
        subroutine validate_unit()
                if ( present(stat) ) then
                    stat = 5
                end if
        end subroutine validate_unit
    end subroutine add_log_unit
end module
program intrinsics_376
    use stdlib_logger_intrinsics_376
    implicit none
  integer :: stat
  call add_log_unit(stat)
  print *, stat
  if (stat /= 5) error stop
end program
