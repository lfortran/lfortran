double precision function mvndfn( w )
   double precision w(*)

 entry mvndnt()
   w(1) = 10.9d0
   w(2) = 10.9d0
   if (abs(w(1) - 10.9d0) > 1e-10) error stop
   if (abs(w(2) - 10.9d0) > 1e-10) error stop
   mvndfn = 1.2d0

end

program entry_06
    interface
        double precision function mvndfn( w )
            double precision w(*)
        end function
    end interface
    double precision w(2)
   if (abs(mvndfn(w) - 1.2d0) > 1e-10) error stop
end program
