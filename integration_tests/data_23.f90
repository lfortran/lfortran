program data_23
    implicit double precision (a-h,o-z)
    character*8 v(4), vfmt(6), v2i
    character*9 v2a

    data v/'('' ''','A16,1PX,',' ','D17.7)'/, &
    & vfmt/'6','17X,5','34X,4','51X,3','68X,2','85X,1'/, &
    & v2a/'A16,1P1X,'/, v2i/'I5,1P2X,'/

    if (trim(v(1)) /= "(' '") error stop "v1"
    if (trim(v(2)) /= "A16,1PX,") error stop "v2"
    if (trim(v(3)) /= "") error stop "v3"
    if (trim(v(4)) /= "D17.7)") error stop "v4"

    if (trim(vfmt(1)) /= "6") error stop "vfmt1"
    if (trim(vfmt(2)) /= "17X,5") error stop "vfmt2"
    if (trim(vfmt(3)) /= "34X,4") error stop "vfmt3"
    if (trim(vfmt(4)) /= "51X,3") error stop "vfmt4"
    if (trim(vfmt(5)) /= "68X,2") error stop "vfmt5"
    if (trim(vfmt(6)) /= "85X,1") error stop "vfmt6"

    if (trim(v2a) /= "A16,1P1X,") error stop "v2a"
    if (trim(v2i) /= "I5,1P2X") error stop "v2i"

    print *, "PASS"
end program
