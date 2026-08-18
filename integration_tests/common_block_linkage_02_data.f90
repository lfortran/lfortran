block data init_name
    implicit none
    character(32) :: name
    common /sn/ name
    data name / 'HELLO' /
end block data init_name
