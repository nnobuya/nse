!***********************************************************************!
!  beta-decay-rate hokanprogram                                         ! 
!             subroutine(blockdata,ecsub,intpol2,seafnn,sebfnn) 02.02.14!
!***********************************************************************!  
module mod_ffn

  implicit none

  public:: ibm, nab, nzb, naq, nzq, ifn, jfn, &
       & ffn_init, ffn_set, sffn !!subroutine
  private

  !..param
  integer, parameter:: nt_ffn   = 13
  integer, parameter:: nrho_ffn = 11


  !..rate set
  integer:: ibm

  real*8 , allocatable    :: raef(:,:,:), ranf(:,:,:), rabf(:,:,:), rapf(:,:,:)
  integer, allocatable    :: nab(:), nzb(:), naq(:), nzq(:), ifn(:), jfn(:)
  character*3, allocatable:: ecap_type(:), pcap_type(:)


  !..constants
  real*8 :: t_ffn(nt_ffn) = &
       & (/ 0.01, 0.10, 0.20, 0.40, 0.70, 1.0, 1.5, &
       &     2.0,  3.0,  5.0, 10.0, 30.0, 100.0 /)
  real*8 :: rho_ffn(nrho_ffn) = &
       & (/ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0 /)

contains


  subroutine ffn_init(io_ecap,io_pcap)

    implicit none

    !..io
    integer :: io_ecap, io_pcap

    !..local
    integer :: i, j, k, ierr


    !..set
    t_ffn(1:nt_ffn) = log10( t_ffn(1:nt_ffn) ) + 9.d0


    !..read
    read(io_ecap,*) ibm
    read(io_pcap,*) ibm

    !..allocation
    allocate ( nab(ibm), nzb(ibm), naq(ibm), nzq(ibm), &
         & ecap_type(1:ibm), pcap_type(1:ibm), &
         & raef(nt_ffn,nrho_ffn,ibm), ranf(nt_ffn,nrho_ffn,ibm), &
         & rabf(nt_ffn,nrho_ffn,ibm), rapf(nt_ffn,nrho_ffn,ibm), stat=ierr )
    if( ierr /= 0 ) stop 'set_ffn(): error'

    do k = 1, ibm
       read(io_ecap,*) nzb(k), nab(k), ecap_type(k)
       read(io_pcap,*) nzq(k), naq(k), pcap_type(k)

       !! (e-,nu)
       read(io_ecap,'(11f8.3)') &
            & ( (raef(i,j,k), j = 1, nrho_ffn), i = 1, nt_ffn )
       read(io_ecap,'(11f8.3)') &
            & ( (ranf(i,j,k), j = 1, nrho_ffn), i = 1, nt_ffn )

       !! (e+,nu')
       read(io_pcap,'(11f8.3)') &
            & ( (rabf(i,j,k), j = 1, nrho_ffn), i = 1, nt_ffn )
       read(io_pcap,'(11f8.3)') &
            & ( (rapf(i,j,k), j = 1, nrho_ffn), i = 1, nt_ffn )
    end do

!    do k = 1, ibm
!       write(100,*) ecap_type(k), pcap_type(k)
!    end do
!    stop 'debug'

    close(io_ecap)
    close(io_pcap)


    return

  end subroutine ffn_init


  subroutine ffn_set

    use mod_const, only: nel
    use mod_set  , only: nz, na

    implicit none

    integer :: i, j, ierr


    allocate(ifn(ibm), jfn(ibm), stat=ierr)
    if( ierr /= 0 ) stop 'ffn_set(): error'

    ifn(1:ibm) = 0.d0
    jfn(1:ibm) = 0.d0
    do i = 1, nel
       do j = 1, ibm
          if( (nzb(j)-1) == nz(i) .and. nab(j) == na(i) ) jfn(j) = i
          if(     nzb(j) == nz(i) .and. nab(j) == na(i) ) ifn(j) = i
       end do
    end do

    return

  end subroutine ffn_set




  ! ------------------------------------------------------------------ !
  !     sffn                                                           !
  !         calculate electron and positron capture rates by FFN       !
  ! ------------------------------------------------------------------ !

  subroutine sffn( te_l, rhye_l, recr, recn, rpcr, rpcn )

    implicit none

    !..io
    real*8, intent(in)  :: te_l, rhye_l
    real*8, intent(out) :: recr(ibm), recn(ibm), rpcr(ibm), rpcn(ibm)

    !..local
    integer :: jb
    real*8  :: ref, rnf

    integer:: mmr, mmt, i_t(4), i_rho(4)
    real*8 :: x_t(4), x_rho(4)



    ! --- read and set of flow: select from FFN data to network (A,Z) ---
    !     recr & recn: ec & n-loss rate ifn->jfn
    !     rpcr & rpcn: pc & n-loss rate jfn->ifn


    do jb = 1, ibm

       call seafnn( te_l, rhye_l, &
            & mmr, mmt, i_t(:), i_rho(:), x_t(:), x_rho(:) )
       !  in: te_l, rhol
       ! out: mmr, mmt, i_t, i_rho, x_t, x_rho


       call sebfnn( jb, 0, te_l, rhye_l, &
            & mmr, mmt, i_t(:), i_rho(:), x_t(:), x_rho(:), &
            & ref, rnf )
       !  in: jbx, jx, tl, mmr, mmt, i_t, i_rho
       ! out: ref, rnf

       recr(jb) = ref   ! e- capture rates
       recn(jb) = rnf   ! n loss rates

       call sebfnn( jb, 1, te_l, rhye_l, &
            & mmr, mmt, i_t(:), i_rho(:), x_t(:), x_rho(:), &
            & ref, rnf )
       !  in: jbx, jx, tl, mmr, mmt, i_t, i_rho
       ! out: ref, rnf

       rpcr(jb) = ref   ! e+ capture rates
       rpcn(jb) = rnf   ! n loss rates
    end do


    return

  end subroutine sffn



  subroutine seafnn( te_l, rhye_l, mmr, mmt, i_t, i_rho, x_t, x_rho )

    implicit none

    !..io
    real*8 , intent(in)  :: te_l, rhye_l
    integer, intent(out) :: mmr, mmt, i_t(4), i_rho(4)
    real*8 , intent(out) :: x_t(4), x_rho(4)


    !..local variables
    integer :: i, j, ij


    mmr = 1
    mmt = 1


    !..search temperature
    if( te_l < t_ffn(3) ) then

       if ( te_l <= t_ffn(2) ) mmt = 2

       do i = 1, 4
          i_t(i) = i
       end do

    else

       loop_t: do i = 3, 11

          if( te_l <= t_ffn(i) ) then

             ij = i - 3
             do j = 1, 4
                i_t(j) = j + ij
             end do

             exit loop_t

          end if

          if( i == 11 ) then

             ij = 9
             do j = 1, 4
                i_t(j) = j + ij
             end do

          end if

       end do loop_t

    end if


    !..search density

    if( rhye_l <= rho_ffn(3) ) then

       if ( rhye_l <= rho_ffn(2) ) mmr = 2
       do i = 1, 4
          i_rho(i) = i
       end do

    else

       loop_rho: do i = 3, 9
          if( rhye_l <= rho_ffn(i)) then

             ij = i - 3
             do j = 1, 4
                i_rho(j) = j + ij
             end do

             exit loop_rho
          end if

          if( i == 9 ) then
             ij = 7
             do j = 1, 4
                i_rho(j) = j + ij
             end do
          end if

       end do loop_rho

    end if



    !..set variables
    x_t(1:4)   =   t_ffn(   i_t(1:4) )
    x_rho(1:4) = rho_ffn( i_rho(1:4) )


    return

  end subroutine seafnn




  subroutine sebfnn( j, mode, te_l, rhye_l, &
       & mmr, mmt, i_t, i_rho, xxt, xxr, ref, rnf )

    implicit none

    !..io
    integer, intent(in)  :: j, mode, mmr, mmt, i_t(4), i_rho(4)
    real*8 , intent(in)  :: te_l, xxt(4), xxr(4), rhye_l
    real*8,  intent(out) :: ref, rnf

    !..local
    integer :: i, k, nt, nr
    real*8  :: f1, f2, ec(4), en(4), ecb(4), enb(4)



    ref = - 50.0
    rnf = - 50.0

    do i = 1, 4
       nr = i_rho(i)

       do k = 1, 4
          nt = i_t(k)

          if( mode == 1 ) then ! mode = 0(ecap) or 1(pcap)
             ec(k) = rabf(nt,nr,j)
             en(k) = rapf(nt,nr,j)
          else
             ec(k) = raef(nt,nr,j)
             en(k) = ranf(nt,nr,j)
          end if
       end do

       if( mmt == 2 ) then
          f1 = ec(1) + (ec(1)-ec(2)) *(te_l - xxt(1)) /(xxt(1) - xxt(2))
          f2 = en(1) + (en(1)-en(2)) *(te_l - xxt(1)) /(xxt(1) - xxt(2))
       else
          f1 = intpol2( xxt, ec, te_l )
          f2 = intpol2( xxt, en, te_l )
       end if

       ecb(i) = f1
       enb(i) = f2
    end do


    if ( mmr == 2 ) then
       ref = ecb(1) + (ecb(1) - ecb(2)) *(rhye_l - xxr(1)) /(xxr(1) - xxr(2))
       rnf = enb(1) + (enb(1) - enb(2)) *(rhye_l - xxr(1)) /(xxr(1) - xxr(2))
    else
       ref = intpol2( xxr, ecb, rhye_l )
       rnf = intpol2( xxr, enb, rhye_l )
    end if


    ref = 10.d0**ref
    rnf = 10.d0**rnf

  
    return

  end subroutine sebfnn




  real*8 function intpol2( x, y, p )

    implicit none

    !..io variables
    real*8, intent(in)  :: x(4), y(4), p


    !..local
    real*8 :: a, b, c
    real*8 :: r, y12, y32, e32, e42


    r = p - x(2)

    y12 = ( y(1) - y(2) ) /( x(1) - x(2) )
    y32 = ( y(3) - y(2) ) /( x(3) - x(2) )

    e32 = (y32 - y12)   /( x(3) - x(1) )
    e42 = ( ( y(4) - y(2) )/( x(4) - x(2) ) - y12 ) /( x(4) - x(1) )

    a = (e42 - e32) /( x(4) - x(3) )
    b = e32 - ( x(3) - 2.d0 *x(2) +  x(1) ) *a
    c = y32 - (a * ( x(3) - x(2) ) + b) *( x(3) - x(2) )

    intpol2  = ( ( a *r + b )*r + c )*r + y(2)


    return

  end function intpol2



end module mod_ffn
