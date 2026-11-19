module mod_out

  use mod_const, only: nel
  use mod_set  , only: n_in, nfreq0, nfreq

  implicit none

  private
  public:: out_main, out_table

contains

  subroutine out_main( istep, time, ye, rho, t9, x, &
       & recy, rpcy, dyedt, dtime, enu )

    implicit none

    integer, intent(in):: istep
    real*8 , intent(in):: time, ye, t9, rho, x(nel), &
         & dyedt, dtime, enu, recy(nel), rpcy(nel)


    if(istep >= 1) then

       write(60,*) istep

!       call out_std( istep, time, rho, t9, ye, x(1:nel) )

!       if ( istep == 1 .or. mod(istep,nfreq) == 0 ) then
!          call out_abund( t9, rho, ye, x(:), recy(:), rpcy(:) )
!          call out_loop( dyedt, dtime, ye, enu )
!          call out_log( x(:), time, rho, t9 *1.d9, ye )
!          call out_flow(63, time, x)
!       end if

!       if ( istep == 1 .or. mod(istep,nfreq0) == 0 ) then
!          call out_nse( 66, istep, time, ye, rho, t9, x(:) )
!       end if

    else
       call out_x_end(64, time, rho, t9, ye, x(:))
       write(*,'(i8,1pe14.5,0pf10.5)') istep, time, ye
       write(*,'(a10,f10.5)') 'Ye_final =', ye
    end if

    return

  end subroutine out_main


  subroutine out_std( istep, time, rho, t9, ye, x )

    use mod_set, only: na, nz

    implicit none

    !..io
    integer, intent(in):: istep
    real*8 , intent(in):: time, rho, t9, ye, x(1:nel)

    !..local
    real*8 :: xn, xp, xl, xh
    integer:: iel


    if( istep == 1 ) then
       write(*,'(9x, "#", 4x, "ti", 8x, "de", 8x, "t9", 7x, "ye", &
            & 7x, "x_n", 4x, "x_p", 4x, "x_l", 4x, "x_h")')
    end if

    if( n_in == 0 .and. ( istep <= 10 .or. mod(istep,500) == 1 ) &
         & .or. ( n_in == 1 .and. mod(istep,1000) == 1 ) ) then

       xl = 0.d0
       xh = 0.d0

       do iel = 1, nel
          if      ( nz(iel) == 0 .and. na(iel) == 1 ) then
             xn = x(iel)
          else if ( nz(iel) == 1 .and. na(iel) == 1 )then
             xp = x(iel)
          else if ( nz(iel) <= 5 ) then
             xl = xl + x(iel)
          else
             xh = xh + x(iel)
          end if
       end do

       write(*,'(i10, 1p3e10.2, 0pf7.4, a2, 4f7.4)') &
            & istep, time, rho, t9, ye, ' |', xn, xp, xl, xh
    end if


    return

  end subroutine out_std



  subroutine out_x_end(io, time, rho, t9, ye, x)

    use mod_set, only: na, nz, name

    implicit none

    !..io
    integer, intent(in):: io
    real*8 , intent(in):: time, rho, t9, ye, x(nel)

    !..tmp
    integer  :: i
    real*8   :: y(nel)
    character:: ch*7


    y(1:nel) = x(1:nel) /dble(na(1:nel))

    write(io,'(a,4x,a4,3(12x,a4))') '#', 'Time', 'Dens', 'Temp', 'Ye  '
    write(io,'(a,1p4e16.8)') '#', time, rho, t9 *1.d9, ye
    write(io,'(a)') '#'

    do i = 1, nel
       ch = adjustl(name(i))
       write(io,'(a7,3i5,1p2e18.8)') &
            & adjustr(ch), na(i) - nz(i), nz(i), na(i), x(i), y(i)
    end do


    return

  end subroutine out_x_end


  subroutine out_nse( io, istep, time, ye, rho, t9, x )

    use mod_set, only: na, nz, name

    implicit none

    !..io
    integer, intent(in):: io, istep
    real*8 , intent(in):: time, ye, rho, t9, x(1:nel)

    if ( istep == 1 ) then
       write(io) nel
       write(io) na(1:nel), nz(1:nel), name(1:nel)
    end if

    write(io) time, ye, rho, t9, x(1:nel)

    return

  end subroutine out_nse




  subroutine out_flow(io, time, x)

    use mod_set, only: name

    implicit none

    !..io
    integer, intent(in):: io
    real*8 , intent(in):: time, x(nel)

    !..local
    real*8  :: xo(nel), total
    integer :: i, j, js, je


    do i = 1, nel
       if( x(i) <= 1.d-30 ) then
          xo(i) = 0.d0
       else
          xo(i) = x(i)
       end if
    end do

    total = sum(xo(1:nel))
    xo(1:nel) = xo(1:nel) /total


    write(io,'(1pe14.5)') time

    do i = 1, nel, 5
       js = i
       je = min(js+4,nel)
       write(io,'(5a14)')  ( adjustr(name(j)), j = js, je )
       write(io,'(1p5e14.5)') ( xo(j), j = js, je )
    end do

    return

  end subroutine out_flow




  subroutine out_log(x, time, rho, temp, ye)

    implicit none

    !..io
    real*8, intent(in):: x(nel), time, rho, temp, ye

    !..tmp
    integer :: jk(nel)
    integer:: i, k, kmx


    k = 0
    do i = 1, nel
       if( log10(x(i)+1.d-10) <= - 4.d0 ) cycle

       k = k + 1
       jk(k) = i
       kmx = k
    end do


    write(60,*)' jmx=', 1
    write(60,*)' kmx=', kmx
    write(61,'(1p4e12.5)') time, rho, temp, ye


    return

  end subroutine out_log


  subroutine out_loop(dyedt, dtime, ye, enu)

    implicit none

    real*8, intent(in):: dyedt, dtime, ye, enu


    write(60,*) '***************'
    write(60,'(1p2e20.10)') dyedt, dtime
    write(60,'(1pe20.10)') ye + dyedt *dtime
    write(60,*) '***************'
    write(60,'(a9,1pe15.7,3x,a11,e15.7)') &
         & 'dyedt=', dyedt, 'epsirn(nu)=', enu

    return

  end subroutine out_loop





  subroutine out_abund(t9, rho, ye, x, recy, rpcy)

    use mod_set, only: na, nz, be, name

    implicit none

    integer, parameter :: iend = 40

    !..io
    real*8, intent(in):: t9, rho, ye
    real*8, intent(in):: x(nel), recy(nel), rpcy(nel)

    integer:: no(nel)
    integer:: i, ii, j, k
    integer:: jq = 0
    real*8 :: tl, rhol, emni, bepq
    character:: ch*6


    rhol     = log10(rho)
    tl       = log10(t9) + 9.d0
    emni     = sum( x(1:nel) /dble(na(1:nel)) )
    bepq     = sum( be(1:nel) *x(1:nel)/dble(na(1:nel)) )



    no(1) = 1

    loop1: do j = 1, nel

       if( x(j) == 1.d0 ) cycle loop1

       loop2: do i = 1, nel
          if( i >= j ) then
             no(j) = j
             cycle loop1
          end if

          if( x(j) > x(no(i)) ) exit loop2
       end do loop2

       do k = j, i + 1, -1
          no(k) = no(k-1)
       end do

       no(i) = j

    end do loop1


    ! -----------------------------
    write(62,'(3x,a3,f8.3,a5,1pe12.5,a4,0pf7.3,a9,f8.3,a10,f8.3)') &
         & 't9=', t9, ' rho=', rho, 'ye=', ye, '  log(t)=', tl, &
         & ' log(rho)=', rhol
    write(62,'(a22)') '0 nse  mass fraction  '
    write(62,'(a4,i3,a9,f8.3,a11,f8.3,x,2(a4,f10.5))') &
         & ' jq=', jq, '  log(t)=', tl, '  log(rho)=', rhol, &
         & ' ye=', ye, ' yi=', emni

    write(62,'(a16,14x,a1,9x,a16)') &
         & '  no   i   z   a', 'x', 'ry(ec)    ry(pc)'

    do i = 1, iend
       ii = i + iend
       j = no(i)
       k = no(ii)
       ch = adjustl(name(j))
       write(62,'(1h ,i3,3i4,a6,1pe15.7,2e10.3)') &
            & i, j, nz(j), na(j), adjustr(ch), x(j), recy(j), rpcy(j)
    end do


    write(62,'(3x,a14,1pe15.7,a8)') &
         & 'average b.e. =', bepq *1.d-3, ' (MeV/n)'

    return

  end subroutine out_abund



  subroutine out_table(istep, rho_lg, t9, ye, x, &
       & enu_ec, enu_pc, dyedt_ec, dyedt_pc, qv_ave)

    use mod_set, only: na, nz

    implicit none

    !..io
    integer, intent(in):: istep
    real*8 , intent(in):: rho_lg, t9, ye, x(nel)
    real*8 , intent(in):: enu_ec, enu_pc, dyedt_ec, dyedt_pc, qv_ave

    real*8 :: x_n, x_p, x_l, x_h
    real*8 :: y(nel), t, a_av, z_av, a2_av, z2_av
    real*8 :: sum

    integer:: i


    if(istep == 1 ) then
       write(65,'("#", 2x, "ye", 13x, "te [K]", 9x, "lg de [g/cc]",     &
            & 3x,"<Z>", 12x, "<Z^2>", 10x, "<A>", 12x, "<A^2>", 10x,    &
            & "<Q>", 12x, "dE(ecap)", 7x, "dE(beta+)", 6x, &
            & "dE(pcap)", 7x, "dE(beta-)", 6x, "dYe(ecap)", 7x, "dYe(beta+)", &
            &  4x, "dYe(pcap)", 6x, "dYe(beta-)", 5x, &
            & "X(n)", 11x, "X(p)", 11x, "X(light)", 7x, "X(heavy)")')
    end if

    y(1:nel) = x(1:nel) /dble(na(1:nel))


    z_av  = 0.d0
    a_av  = 0.d0
    z2_av = 0.d0
    a2_av = 0.d0
    sum   = 0.d0

    do i = 1, nel
       if ( nz(i) >= 6 ) then
          sum = sum + y(i)
          a_av  = a_av  + dble( na(i) ) *y(i)
          z_av  = z_av  + dble( nz(i) ) *y(i)
          a2_av = a2_av + dble( na(i) *na(i) ) *y(i)
          z2_av = z2_av + dble( nz(i) *nz(i) ) *y(i)
       end if
    end do

    a_av  = a_av /sum
    z_av  = z_av /sum
    a2_av = a2_av /sum
    z2_av = z2_av /sum

    x_n = 0.d0
    x_p = 0.d0
    x_l = 0.d0
    x_h = 0.d0

    do i = 1, nel
       if      ( nz(i) == 0 .and. na(i) == 1 ) then
          x_n = x(i)
       else if ( nz(i) == 1 .and. na(i) == 1 ) then
          x_p = x(i)
       else if ( nz(i) <= 5 ) then
          x_l = x_l + x(i)
       else
          x_h = x_h + x(i)
       end if
    end do


    t = t9 *1.d9

    write(65,'(1p20e15.7)') &
         & ye, t, rho_lg, z_av, z2_av, a_av, a2_av, qv_ave, &
         & enu_ec, 0.d0, enu_pc, 0.d0, dyedt_ec, 0.d0, dyedt_pc, 0.d0, &
         & x_n, x_p, x_l, x_h


    return

  end subroutine out_table


end module mod_out
