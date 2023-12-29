module m
    implicit none
    contains
    function all_true(tf,idim) result(vec)
    logical, intent(in) :: tf(:,:)
    integer, intent(in) :: idim
    logical             :: vec(size(tf,dim=3-idim))
    vec = .true.
    end function all_true
end module m
program intrinsics_99
end
