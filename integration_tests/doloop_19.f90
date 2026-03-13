! This code intends to check stack memory leaks inside do-loop.

program do_loop_19

    implicit none
    integer :: isum, i, j, ans
    integer,dimension(:,:),allocatable :: price2
    integer,dimension(:,:,:),allocatable :: perimeter

    integer, parameter :: NTOTAL = 3
    allocate(price2(NTOTAL, NTOTAL))

    if (.not. allocated(perimeter)) then
        allocate(perimeter(4, NTOTAL, NTOTAL)); perimeter = 0
    end if

    do i = 1, NTOTAL
        do j = 1, NTOTAL
            call go()
            price2(i,j) = compute_n_sides()
            print *,"i", i, j
        end do
    end do
    ans = sum(price2)
    print *,ans
    if (ans /= 324) error stop
    

    contains
        recursive subroutine go()
            perimeter = 1     
        end subroutine go

        function compute_n_sides() result(iper)
            integer :: iper
            iper = count(perimeter==1)
        end function compute_n_sides

end program do_loop_19