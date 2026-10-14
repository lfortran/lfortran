      subroutine dsqrt_kind_error(ng, rad, rsw)
      use atommod
      implicit none
      include 'params.inc'
      integer ng
      double precision rad, rsw
      rsw = rad * dsqrt(14.0/ng)
      end
