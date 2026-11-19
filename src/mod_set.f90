module mod_set

  use mod_const, only: nel

  implicit none

  private
  public:: na, nz, nn, ra, rz, rn, name, be, qm, spin, npt, cpt, ct9, &
       & nbet, neu, ndt, n_in, nfreq0, nfreq, &
       & rti, rrh, rte, rye, rrh_lg, rrd, &
       & temp_lim, t_min, dt_init, ti_init, ye_init, &
       & set_nuclei, set_rhot, set_init, qm_n, qm_p, &
       & r_neu

  !..rhot
  integer:: ndt
  integer:: n_in, neu, nfreq0, nfreq
  real*8 :: ti_init, dt_init, ye_init
  real*8 :: temp_lim, t_min !! limit value
  real*8, allocatable :: rti(:), rrh(:), rte(:), rye(:), rrh_lg(:), rrd(:)

  !..neutrino
  real*8, allocatable :: r_neu(:,:)

  !..form bet
  integer   :: nbet

  integer:: nn(1:nel), na(1:nel), nz(1:nel)
  real*8 :: qm_n, qm_p
  real*8 :: rn(1:nel), ra(1:nel), rz(1:nel)
  real*8 :: be(1:nel), qm(1:nel), spin(1:nel)

  integer:: npt
  real*8, allocatable:: cpt(:,:), ct9(:)
  character :: name(1:nel)*14

   !-----data for network-construction
   !     imas : left hanside mass number
   !     jel  : number of nuclei for given z
   !     izr  : number to reduce p-rich nuclei for given z
   !     inr  : number to reduce n-rich nuclei for given z


contains


  subroutine set_rhot(io)

    implicit none

    integer, intent(in):: io

    real*8 :: r1
    integer:: i, j, ier


    if ( n_in == 0 ) then
       ndt = 0
    else
       ndt  = -1
    end if

    lp_ndt: do
       read(io,*,iostat = ier )

       if(ier /= 0) exit lp_ndt

       ndt = ndt + 1
    end do lp_ndt


    allocate ( rti(1:ndt), &
         & rrh(1:ndt), rrh_lg(1:ndt), rte(1:ndt), rye(1:ndt), &
         & rrd(1:ndt), stat = ier)
    if(ier /= 0) stop 'qnse(): error'

    !! neutrino
    allocate( r_neu(1:ndt,1:6), stat = ier)
    if(ier /= 0) stop 'set_nuclei(): error '


    if( n_in == 0 ) then
       rewind(io)
       do i = 1, ndt
          read(io,*) rti(i), rrh(i), rte(i), rrd(i)
       end do
       rye(1:ndt) = 0.d0

       !!neutrino
       if( neu == 1 ) then
          do i = 1, ndt
             read(52,*) r1, ( r_neu(i,j), j = 1, 6 )
          end do
       end if

    else
       rewind(io)
       read(io,*)
       do i = 1, ndt
          read(io,*) rye(i), rte(i), rrh_lg(i)
       end do

       rte(1:ndt) = rte(1:ndt) *1.d9
       rrh(1:ndt) = 10.d0**rrh_lg(1:ndt)

       rti(1:ndt) = 0.d0
       rrd(1:ndt) = 0.d0
    end if

    close(io)

    return

  end subroutine set_rhot



  subroutine set_init(io)

    implicit none

    !..io
    integer, intent(in) :: io

    read(io,*)
    read(io,*)
    read(io,*) nbet
    read(io,*)
    read(io,*) neu
    read(io,*)
    read(io,*) n_in
    read(io,*)
    read(io,*) nfreq0, nfreq
    read(io,*)
    read(io,*)
    read(io,*)
    read(io,*) temp_lim
    read(io,*)
    read(io,*) t_min
    read(io,*)
    read(io,*) ye_init
    read(io,*)
    read(io,*) ti_init
    read(io,*)
    read(io,*) dt_init

    close(io)


    return

  end subroutine set_init


  subroutine set_nuclei

    implicit none

    integer :: i, j, k, js, je, ier


    qm_n = 0.d0
    qm_p = 0.d0

    read(40,*) npt

    allocate( ct9(1:npt), cpt(1:npt,1:nel), stat = ier)
    if(ier /= 0) stop 'set_nuclei(): error '


    !..t9 grid
    do i = 1, npt, 12
       read(40,*) ( ct9(j), j = i, i + 11 )
    end do


    !..pt func.
    read(40,*) j
    if (j /= nel) stop 'error'

    do i = 1, nel
       read(40,*) name(i), na(i), nz(i), nn(i), spin(i), qm(i)
       do k = 1, npt, 8
          js = k
          je = k + 7
          read(40,*) ( cpt(j,i), j =  js,  je )
       end do
    end do
    close(40)

    cpt(1:npt,1:nel) = log10(cpt(1:npt,1:nel))

    ra(1:nel) = dble(na(1:nel))
    rz(1:nel) = dble(nz(1:nel))
    rn(1:nel) = dble(nn(1:nel))

    do i = 1, nel
       if(na(i) == 1 .and. nz(i) == 0) qm_n = qm(i)
       if(na(i) == 1 .and. nz(i) == 1) qm_p = qm(i)

       if( qm_n /= 0 .and. qm_p /= 0 ) exit

    end do

    if(qm_n == 0 .and. qm_p == 0) stop 'set_nuclei(): no p or n'

    be(1:nel) = rn(1:nel) *qm_n + rz(1:nel) *qm_p - qm(1:nel)


    return

  end subroutine set_nuclei



end module mod_set
