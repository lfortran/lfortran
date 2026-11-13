program external_func_01
character(len=1) :: kind
logical :: lsame
external :: lsame
kind = 'D'
if (lsame(kind, 'D')) then
    print *, 'ok'
else
    print *, 'bad'
end if
end program
