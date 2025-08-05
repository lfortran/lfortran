program array_03
    integer, parameter :: RP = kind(0.0D0)
    integer, parameter :: IK = kind(0)
    real(RP) :: G(2, 2)
    integer(IK) :: knew
    real(RP):: zmat(1:1, 1:1) 
    knew = 1
    zmat = reshape([1.0_RP], [1, 1])
    G = planerot(zmat(knew, [1_IK, 1]))
    print *, G
    if (any(G /= reshape([1.0_RP, -1.0_RP, 1.0_RP, 1.0_RP], [2, 2]))) error stop
    contains
    
    function planerot(x) result(G)
        real(RP), intent(in) :: x(:)
        real(RP) :: c, s, r
        real(RP) :: G(2, 2)
        r = 1
        s = 1
        c = x(1) / r
        G = reshape([c, -s, s, c], [2, 2])
    end function
end program
