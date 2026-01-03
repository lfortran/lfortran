program intrinsics_399
    debugit: block
    integer :: j, ilen
    character(len=256) :: big_argument
    write(*,*)'arguments seen directly by program'
    do j=1,command_argument_count()
        call get_command_argument(number=j,value=big_argument,length=ilen)
        write(*,'(*(g0))')j,'[',big_argument(:ilen),']'
    end do
    end block debugit
end program intrinsics_399