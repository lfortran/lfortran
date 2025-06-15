program if_label_01
    implicit none

    integer :: basein_local
    basein_local = 1  ! Try 1 or 2 to test both branches

ALL: if (basein_local < 0) then
        print *, "Inside IF block: basein_local =", basein_local
    else
        TMP: block 
            basein_local = 2  ! This will override the outer variable
            exit ALL
        end block TMP
        print *, "Inside ELSE block: basein_local =", basein_local
        basein_local = 3  ! This will not be executed if the exit is taken
    end if ALL
    print *, "After IF block: basein_local =", basein_local
    if (basein_local /= 2) error stop
end program if_label_01
