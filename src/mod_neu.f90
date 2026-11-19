module mod_neu

  use mod_const, only: pi

  implicit none

  real*8 , parameter:: delta = 1.293d0, Gf = 1.16639d-7
  real*8 , parameter:: a = 1.26d0


contains


  real*8 function r_weak_qw( mode, r_nu, l, e, rad )

    implicit none

    integer, intent(in):: mode
    real*8 , intent(in):: r_nu, l, e, rad

    real*8 :: x


    if( rad > r_nu) then
       x = sqrt( 1.d0 - (r_nu /rad)**2 )
       r_weak_qw = ( (1.d0 + 3.d0*a**2) /2.d0 /pi**2 ) &
            & * Gf**2 *( L /r_nu**2 ) &
            & * ( E + dble(mode) *2.d0 * delta + 1.2d0 *delta**2 /E) &
            & * (1.d0 - x)
    else
       r_weak_qw = 0.d0
    end if

    return

  end function r_weak_qw



  real*8 function r_weak_qw2( mode, r_nu, l_in, e, rad_in )

    implicit none

    integer, intent(in):: mode
    real*8 , intent(in):: r_nu, e, l_in, rad_in

    real*8:: l, rad

    L   =   L_in *1.d-51
    rad = rad_in *1.d-6

    r_weak_qw2 = &
         & 4.83d0 *L &
         & *(E + dble(mode) *2.d0*delta + 1.2d0 * delta**2 /E ) /rad /rad


    return

  end function r_weak_qw2




end module mod_neu
