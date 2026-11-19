subroutine neutrino( time, ye, rad, dyedt_np, dyedt_pn, istep )

  use mod_set, only: r_neu
  use mod_neu, only: r_weak_qw, r_weak_qw2

  implicit none

  !..io
  integer, intent(in):: istep
  real*8, intent(in) :: time, ye, rad
  real*8, intent(out):: dyedt_np, dyedt_pn


  real*8 :: r_nu    , l_nu    , e_nu
  real*8 :: r_nu_bar, l_nu_bar, e_nu_bar

  integer:: j


  r_nu     = r_neu(istep,1)
  r_nu_bar = r_neu(istep,2)
  l_nu     = r_neu(istep,3)
  l_nu_bar = r_neu(istep,4)
  e_nu     = r_neu(istep,5)
  e_nu_bar = r_neu(istep,6)

  dyedt_np = r_weak_qw2(   1, r_nu    , l_nu    , e_nu    , rad )
  dyedt_pn = r_weak_qw2( - 1, r_nu_bar, l_nu_bar, e_nu_bar, rad )

  dyedt_np =   ( 1.d0 - ye ) *dyedt_np
  dyedt_pn =          - ye   *dyedt_pn


  return

end subroutine neutrino
