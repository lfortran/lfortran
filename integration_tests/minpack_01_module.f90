module minpack_01
    implicit none
    public 
    interface
        double precision function enorm1(n) result(y)
            double precision n
        end function
    end interface
end module
