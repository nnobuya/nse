! ==================================================================== !
!                                                                      !
!    Waoger electron capture rates                                     !
!                                                                      !
!       2008.06.20                                                     !
!                                                                      !
! ==================================================================== !

module mod_wagoner

  implicit none

  private
  public:: wagoner_np, wagoner_pn

  !..constants
  real*8, parameter :: mn  = 1.6749286d-24
  real*8, parameter :: mp  = 1.6726231d-24
  real*8, parameter :: me  = 9.1093897d-28
  real*8, parameter :: c   = 2.99792458d10
  real*8, parameter :: kb  = 1.380658d-16
  real*8, parameter :: tau = 885.7d0

contains

  real*8 function wagoner_np(T)

    implicit none

    real*8, intent(in)  :: T
    real*8 :: z


    z   = me *c *c /kb /T

    wagoner_np = 1.d0 /tau *(1.d0 + 0.565d0 /z - 6.382d0 /z /z &
         & + 11.108d0 /z /z /z + 36.492d0 /z /z /z /z &
         & + 27.512d0 /z /z /z /z /z )

    return

  end function wagoner_np



  real*8 function wagoner_pn(T)

    implicit none

    real*8, intent(in)  :: T
    real*8 :: z, q


    z = me *c *c /kb /T
    q = ( mn - mp ) /me

    z = min(z, 20.d0)

    wagoner_pn = 1.d0 /tau *(5.252d0 /z - 16.229d0 /z /z + 18.059d0 /z /z /z &
         & + 34.181d0 /z /z /z /z + 27.617d0 /z /z /z /z /z ) * exp( - q *z )

    wagoner_pn = max( 1.d-30, wagoner_pn)

    return

  end function wagoner_pn


end module mod_wagoner



subroutine wagoner_plot

  use mod_wagoner, only : wagoner_np, wagoner_pn

  implicit none

  real*8  :: rate1, rate2
  real*8  :: linT, T, Tini, Tend, dT, Te


  !..the range of temperature and differences
  Tini = 7.d0
  Tend = 11.d0

  dT = (Tend - Tini) /100.d0
  !..end


  !..main loop
  T = Tini

  plotdata: do
     if( T >= Tend ) exit

     linT = 10.d0**T

     rate1 = wagoner_np(linT)
     rate2 = wagoner_pn(linT)

     Te = ( 10.d0**T ) *1.d-10

     write(*,'(1p3e14.5)') Te, rate1, rate2

     T = T + dT

  end do plotdata
  !..end


  stop

end subroutine wagoner_plot
