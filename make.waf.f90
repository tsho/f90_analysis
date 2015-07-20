program main
  implicit none

!  integer,parameter :: &
!       & defx=144,defy=73,&
!       & deft=540,syr=1,eyr=63,defyr=63,dyr=eyr-syr+1
! syr=1 == year1948, syr=15 year=1962
! eyr=63 year=2010, eyr=29 year=1977

  real,parameter :: &
       & undef_old=-9.99E8,a=6.37E6, &
       & omega=7.292E-5, g=9.80665,cp=1004,Rd=287!
!       & dx=360/defx, dy=180/(defy-1)

  real(4), allocatable ::  &
       & u_clim       (:,:,:), &
       & v_clim       (:,:,:), &
       & z_ano       (:,:,:), &
       & z_clim       (:,:,:), &
       & psi_ano       (:,:,:), &
       & psic       (:,:,:), &
       & t_clim       (:,:,:), &
       & pt_clim   (:,:,:), &
       & wafx       (:,:,:), &
       & wafy       (:,:,:), &
       & wafz       (:,:,:), &
       & wafz_p  (:,:,:), &
       & plev (:),logz(:),&
       & psi_x(:,:,:),&
       & psi_y(:,:,:), &
       & psi_xy(:,:,:), &
       & u1(:,:,:), &
       & u2(:,:,:), &
       & u3(:,:,:), &
       & u4(:,:,:), &
       & v1(:,:,:), &
       & v2(:,:,:), &
       & v3(:,:,:), &
       & v4(:,:,:), &
       & z1(:,:,:), &
       & z2(:,:,:), &
       & z3(:,:,:), &
       & z4(:,:,:), &
       & q(:,:,:), &
       & k2(:,:,:)

!       & pv(:,:,:), &


  real (4) ::  undef, r, pi, &
       & psi_xx, psi_yy,&
       & psi_z, psi_yz, psi_xz, &
       & psi_p, psi_xp, psi_yp, &
       & psic_xx, psic_yy, psic_zz, delq,&
       & wind, N2,S2, H, &
       & lat, dx, dy, &
       & dPTdz,dlnPTdP, termxU, termxV, termyU, termyV, termzU, termzV, &
       & pcos, p, dz, dp, termpU, termpV, &
       & paramxU, paramxV, paramyU, paramyV, paramzU, paramzV, zparam, &
       & parampU, parampV, pparam, alpha, pU, pV,&
       & xU, xV, yU, yV, zU, zV, f, f0, kappa, ppt, &
       & dNdz,dN2ddz

  real (4) ::  &
       &   lev, pru, prl, N2u, N2l, dPTdzu, dPTdzl, &
       & k2_1, k2_2, k2_3, k2_4

  integer :: ix,iy,iz,it,k,j,&
       &  l

  character(150) :: FileName, plevfile

  integer :: ifnum, ifnum2, n,imo2,  mocom, &
       & defx, defy, defz, lev_num, surface, syr,eyr,dyr, z_lev, lag

!  data (plev(k),k=1,defz) &
!       &/1000 ,925, 850, 700, 600, &
!       & 500, 400, 300, 250, 200,  &
!       & 150, 100,  70,  50,  30,  &
!       & 20, 10  /


!
!  !!!!!!!!!  intialization  !!!!!!!!!!!!!!!!!!!!!!!!
!
  read(*,*) undef
  read(*,*) defx
  read(*,*) defy
  read(*,*) defz
  read(*,*) syr
  read(*,*) eyr
  read(*,*) dyr
  read(*,*) lev
  read(*,*) z_lev
!  read(*,*) surface
  read(*,*) lag
  read(*,*) mocom
  read(*,'(a150)') plevfile

  if( surface ==1)then
     defz=1
  endif

  write(*,*) undef
  write(*,*) 'defx',defx
  write(*,*) 'defy',defy
  write(*,*) 'defz',defz
  write(*,*) 'start year',syr
  write(*,*) 'end year',eyr
  write(*,*) 'end year',dyr
  write(*,*) 'lev', lev
  write(*,*) 'z_levber e.g. z=1000 z_lev=1',z_lev
!  write(*,*) 'surface',surface
  write(*,*) 'lag',lag
  write(*,*) 'monthly composite',mocom
  write(*,*) plevfile

! climatology clocatable
  allocate ( &
       & u_clim       (defx,defy,defz), &
       & v_clim       (defx,defy,defz), &
       & z_ano       (0:defx+1,defy,defz), &
       & z_clim       (0:defx+1,defy,defz), &
       & psi_ano       (0:defx+1,defy,defz), &
       & psic       (0:defx+1,defy,defz), &
       & t_clim       (defx,defy,defz), &
       & pt_clim   (defx,defy,defz), &
       & wafx       (defx,defy,defz), &
       & wafy       (defx,defy,defz), &
       & wafz       (defx,defy,defz), &
       & wafz_p       (defx,defy,defz), &
       & plev (defz),logz(defz),&
       & psi_x (defx,defy,defz),&
       & psi_y (defx,defy,defz), &
       & psi_xy (defx,defy,defz),&
       & u1 (defx,defy,defz), &
       & u2 (defx,defy,defz), &
       & u3 (defx,defy,defz), &
       & u4 (defx,defy,defz), &
       & v1 (defx,defy,defz), &
       & v2 (defx,defy,defz), &
       & v3 (defx,defy,defz), &
       & v4 (defx,defy,defz), &
       & z1 (defx,defy,defz), &
       & z2 (defx,defy,defz), &
       & z3 (defx,defy,defz), &
       & z4 (defx,defy,defz), &
       & k2 (defx,defy,defz), &
       & q (defx,defy,defz) )

  open(1000, file=plevfile)
  read(1000,*)
  do iz=1,defz
     read(1000,*) plev(iz)
  enddo

  write(*,*) plev(1:defz)

!
! !!!!!!!!1111    I/O file data     !!!!!!!!!!!!!!!!!!!!11
!
! Input file, Z
  read(*,'(a150)') FileName
  write(*,*) FileName
  open (10, &
       & file=FileName,&
       & form='unformatted',access="direct",CONVERT='little_ENDIAN',&
       & recl=(defx)*(defy)*4*defz)
!T
  read(*,'(a150)') FileName
  write(*,*) FileName
  open (12, &
       & file=FileName,&
       & form='unformatted',access="direct",CONVERT='little_ENDIAN',&
       & recl=(defx)*(defy)*4*defz)

! U
  read(*,'(a150)') FileName
  write(*,*) FileName
  open (14, &
       & file=FileName,&
       & form='unformatted',access="direct",CONVERT='little_ENDIAN',&
       & recl=(defx)*(defy)*4*defz)

! V
  read(*,'(a150)') FileName
  write(*,*) FileName 
  open (16, &
       & file=FileName,&
       & form='unformatted',access="direct",CONVERT='little_ENDIAN',&
       & recl=(defx)*(defy)*4*defz)


! Output file 
  read(*,'(a150)') FileName
  write(*,*) FileName 
  open (100, &
       & file=FileName, & 
       & form='unformatted',access="direct",CONVERT='little_ENDIAN',&
       & recl=(defx)*(defy)*4*defz)


  write(*,*) "******************"
  write(*,*) "read data complete"
  write(*,*) "******************"


  write(*,*) "******************"


  read(10,rec=1) (((z_ano(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(10,rec=1+4) (((z_clim(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(12,rec=1+4) (((t_clim(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
!  read(14,rec=1) (((u_ano(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
!  read(16,rec=1) (((v_ano(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(14,rec=1+4) (((u_clim(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(16,rec=1+4) (((v_clim(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)

!  u_clim(:,:,:)=0.d0


  call calc_waf

! output for anomaly data.
  write(100,rec=1) (((wafx(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=2) (((wafy(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=3) (((wafz(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=4) (((wafz_p(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=5) (((u1(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=6) (((u2(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=7) (((u3(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=8) (((u4(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=9) (((v1(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=10) (((v2(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=11) (((v3(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=12) (((v4(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=13) (((z1(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=14) (((z2(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=15) (((z3(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=16) (((z4(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=17) (((k2(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  
!initialization 
  z_ano=0.d0
  t_clim=0.d0
  u_clim=0.d0
  v_clim=0.d0


! calc WAf in Negative Phase
! read data composite file in Negative Phase
  read(10,rec=1+1) (((z_ano(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(10,rec=1+4) (((z_clim(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(12,rec=1+4) (((t_clim(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(14,rec=1+4) (((u_clim(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(16,rec=1+4) (((v_clim(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)

  call calc_waf

!number of variable
  l=17

  write(100,rec=1+l) (((wafx(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=2+l) (((wafy(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=3+l) (((wafz(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=4+l) (((wafz_p(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=5+l) (((u1(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=6+l) (((u2(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=7+l) (((u3(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=8+l) (((u4(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=9+l) (((v1(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=10+l) (((v2(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=11+l) (((v3(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=12+l) (((v4(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=13+l) (((z1(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=14+l) (((z2(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=15+l) (((z3(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=16+l) (((z4(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  write(100,rec=17+l) (((k2(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)


  close(10)
  close(12)
  close(14)
  close(16)
  close(100)



  stop
contains


subroutine calc_waf
  implicit none

  real,parameter :: &
       & undef_old=-9.99E8,a=6.37E6, &
       & omega=7.292E-5, g=9.80665,cp=1004.d0,Rd=287.d0!

  integer :: ix,iy,iz,it,k,j,n, l

  character(150) :: FileName, plevfile

  integer :: ifnum, ifnum2, imo2,  mocom, &
       & lev_num, surface, syr,eyr,dyr, z_lev, lag


!================================================!
!       scale height, dx, dy , logp              !
!  convert to logz coodinate from p coodinate    !
! ============================================== !
  pi=acos(-1.d0)

  H = Rd * (-15.d0 + 273.2) / g
  dx = ( real(360, 4) / real(defx, 4) ) * pi / 180.d0
  dy = real(180, 4) / real((defy - 1), 4) * pi / 180.d0
  f0 = 2.d0 * Omega * sin(43.d0 * pi / 180.d0)
  kappa = Rd / Cp

  logz(1) =0.d0

  do k=2,defz
     logz(k) = -H * (log( real( plev(k) / 1000.d0, 4)))
  enddo

!  write(*,*) logz(pu),logz(pm),logz(pl)
!  write(*,*) plev(pu),plev(pm),plev(pl)

! ===================================================== !
!     convert to strm from geopotential height          !
! ===================================================== !
  do iz=1, defz
     ppt = 1000.d0 / (plev(iz))
     do ix=1, defx
        do iy=1, defy
           lat = (pi * ((real(iy - 1) * 2.50) - 90.d0) / 180.d0)
           f = 2.d0 * Omega * sin(lat)

           pt_clim(ix,iy,iz) = (t_clim(ix,iy,iz) + 273.15) * &
             & (ppt ** kappa)

           psi_ano(ix,iy,iz) = (z_ano(ix,iy,iz) * g) / f
           psic(ix,iy,iz) = (z_clim(ix,iy,iz) * g) / f

! ============================================ !
! set to boudary condition
! ============================================ !
           if(t_clim(ix,iy,iz) == undef)then
              pt_clim(ix,iy,iz) = undef
           endif
           if(ix == 1)then
              psi_ano(defx+1,iy,iz) = psi_ano(1,iy,iz)
              psic(defx+1,iy,iz) = psic(1,iy,iz)
           elseif(ix == defx)then
              psi_ano(0,iy,iz) = psi_ano(defx,iy,iz)
              psic(0,iy,iz) = psic(defx,iy,iz)
           endif
        enddo
     enddo
  enddo

!
! ---------- q, potential vorticity -------------
!


  do ix=1, defx
     do iy=2, defy-1
        do iz=1+1, defz-1

           lat = (pi * ((real(iy - 1) * 2.50) - 90.d0) / 180.d0)
           f = 2.d0 * Omeg a* sin(lat)

           psic_xx = (psic(ix+1,iy,iz) + (-2.d0 * psic(ix,iy,iz)) + psic(ix-1,iy,iz)) / (dx*dx)
           psic_yy = (psic(ix,iy+1,iz) + (-2.d0 * psic(ix,iy,iz)) + psic(ix,iy-1,iz)) / (dy*dy)

           q(ix,iy,iz) = f+psic_xx + psic_yy
        enddo
     enddo
  enddo

! =============================================== 1
!         cal psi_x, psi_y, psi_xy                    !
! =============================================== !
  do iz = 1, defz
     do ix = 1, defx
        do iy = 1, defy
           psi_x(ix,iy,iz) = (psi_ano(ix+1,iy,iz) - psi_ano(ix-1,iy,iz)) / (2 * dx)
           if(iy==1 )then
              psi_y(ix,iy,iz) = (psi_ano(ix,iy+1,iz) - psi_ano(ix,iy,iz)) / (dy)
           elseif(iy == defy)then
              psi_y(ix,iy,iz) = (psi_ano(ix,iy,iz)-psi_ano(ix,iy-1,iz))/(dy)
           else
              psi_y(ix,iy,iz) = (psi_ano(ix,iy+1,iz)-psi_ano(ix,iy-1,iz))/(2 * dy)
           endif
        enddo
     enddo

     do ix=1, defx
        do iy=1, defy
           if(iy==1)then
              psi_xy(ix,iy,iz) = (psi_x(ix,iy+1,iz)-psi_x(ix,iy,iz))/dy
           elseif(iy==defy)then
              psi_xy(ix,iy,iz) = (psi_x(ix,iy,iz)-psi_x(ix,iy-1,iz))/dy
           else
              psi_xy(ix,iy,iz) = (psi_x(ix,iy+1,iz)-psi_x(ix,iy-1,iz)) / (2.d0 * dy)
           endif
        enddo
     enddo
  enddo


! ============================================ !
! calculate wave activity flux                  !
! ===========================================  !
  do ix = 1, defx
     do iy = 2, defy-1
        do iz = 1, defz
           lat = (pi * ((real(iy - 1) * 2.50) - 90.d0) / 180.d0)

           psi_xx = (psi_ano(ix+1,iy,iz) + (-2.d0 * psi_ano(ix,iy,iz)) + psi_ano(ix-1,iy,iz))/( dx**2 )
           psi_yy = (psi_ano(ix,iy+1,iz) + (-2.d0 * psi_ano(ix,iy,iz)) + psi_ano(ix,iy-1,iz))/( dy**2 )

           if(iz == 1)then
              dz = (logz(iz+1) - logz(iz))
              dPTdz = (pt_clim(ix,iy,iz+1) - pt_clim(ix,iy,iz)) / dz
              psi_z  = (psi_ano(ix,iy,iz+1) - psi_ano(ix,iy,iz)) / dz
              psi_xz  = (psi_x(ix,iy,iz+1) - psi_x(ix,iy,iz)) / dz
              psi_yz  = (psi_y(ix,iy,iz+1) - psi_y(ix,iy,iz)) / dz
              dp = (plev(iz+1) - plev(iz))
              dlnPTdp = (log(pt_clim(ix,iy,iz+1)) - log(pt_clim(ix,iy,iz)))/dp
              psi_p  = (psi_ano(ix,iy,iz+1) - psi_ano(ix,iy,iz))/dp
              psi_xp  = (psi_x(ix,iy,iz+1) - psi_x(ix,iy,iz))/dp
              psi_yp  = (psi_y(ix,iy,iz+1) - psi_y(ix,iy,iz))/dp
           elseif(iz == defz)then
              dz =  (logz(iz) - logz(iz-1))
              dPTdz = (pt_clim(ix,iy,iz) - pt_clim(ix,iy,iz-1)) / dz
              psi_z  = (psi_ano(ix,iy,iz) - psi_ano(ix,iy,iz-1)) / dz
              psi_xz  = (psi_x(ix,iy,iz) - psi_x(ix,iy,iz-1)) / dz
              psi_yz  = (psi_y(ix,iy,iz) - psi_y(ix,iy,iz-1)) / dz
              dp = (plev(iz) - plev(iz-1))
              dlnPTdp = (log(pt_clim(ix,iy,iz)) - log(pt_clim(ix,iy,iz-1))) / dp
              psi_p  = (psi_ano(ix,iy,iz) - psi_ano(ix,iy,iz-1)) / dp
              psi_xp  = (psi_x(ix,iy,iz) - psi_x(ix,iy,iz-1)) / dp
              psi_yp  = (psi_y(ix,iy,iz) - psi_y(ix,iy,iz-1)) / dp
           else
              dz = (logz(iz+1) - logz(iz-1))
              dPTdz = (pt_clim(ix,iy,iz+1) - pt_clim(ix,iy,iz-1)) / dz
              psi_z  = (psi_ano(ix,iy,iz+1) - psi_ano(ix,iy,iz-1)) / dz
              psi_xz  = (psi_x(ix,iy,iz+1) - psi_x(ix,iy,iz-1)) / dz
              psi_yz  = (psi_y(ix,iy,iz+1) - psi_y(ix,iy,iz-1)) / dz
              dp = (plev(iz+1) - plev(iz-1))
              dlnPTdp = (log(pt_clim(ix,iy,iz+1)) - log(pt_clim(ix,iy,iz-1))) / dp
              psi_p  = (psi_ano(ix,iy,iz+1) - psi_ano(ix,iy,iz-1)) / dp
              psi_xp  = (psi_x(ix,iy,iz+1) - psi_x(ix,iy,iz-1)) / dp
              psi_yp  = (psi_y(ix,iy,iz+1) - psi_y(ix,iy,iz-1)) / dp

              if(iz>= 3 .and. iz<=defz-2)then
                 dPTdzu = (pt_clim(ix,iy,iz+2) - pt_clim(ix,iy,iz))/ &
                      & (logz(iz+2) - logz(iz))
                 dPTdzl = (pt_clim(ix,iy,iz) - pt_clim(ix,iy,iz-2))/ &
                 & (logz(iz) - logz(iz-2))
            endif

           endif


           p=(plev(iz)/1000.d0)

           N2  = (Rd * (p ** kappa) / H) * dPTdz
           N2u = (Rd * (pru ** kappa) / H) * dPTdzu
           N2l = (Rd * (prl ** kappa) / H) * dPTdzl

           alpha = Rd * (t_clim(ix,iy,iz) + 273.15) / plev(iz)
           S2 = -1.d0 * alpha * dlnPTdp

           if(N2 < 0.d0)then
              N2 = undef
           endif

           if(S2 < 0.d0)then
              S2 = undef
           endif

           f = 2.d0 * Omega * sin(lat)
           zparam = (f**2) / N2
           pparam = (f**2) / S2

           wind=sqrt( (u_clim(ix,iy,iz)**2) + (v_clim(ix,iy,iz)**2))
           pcos = p*cos(lat) / (2.d0*wind)


           ! refrective index
           if(iz < 3 .or. iz > defz-2 .or. ix == 1 .or. ix == defx)then
              k2(ix,iy,iz) = undef
           else
              pru = (plev(iz+1) / 1000.d0)
              prl = (plev(iz-1) / 1000.d0)

              dNdz = (sqrt(1.d0 / N2u) - sqrt(1.d0 / N2l)) / dz
              dNdz = (sqrt(1.d0 / N2u) - 2.d0 * sqrt(1.d0 / N2) + sqrt(1.d0 / N2l)) / (dz * 2.d0)

              delq = (( q(ix+1,iy,iz) - q(ix-1,iy,iz)) / (2.d0 * dx) +&
                   & (( q(ix,iy+1,iz) - q(ix,iy-1,iz)) / (2.d0 * dy))

              k2_1 = abs(delq) / abs(wind)
              k2_2 = (f**2) / (4 * N2 * H * H)
              k2_3 = (4.d0 * H * sqrt(N2) * dNdz)
              k2_4 = 4.d0 * H * H * sqrt(N2) * dN2ddz

              k2(ix,iy,iz)=k2_1 -(k2_2*(1.d0-k2_3+k2_4))
              k2(ix,iy,iz)=q(ix,iy,iz)
           endif

           paramxU = u_clim(ix,iy,iz) / ((a**2) * (cos(lat) **2))
           paramxV = v_clim(ix,iy,iz) / ((a**2) *  cos(lat))
           xU      = ((psi_x(ix,iy,iz)**2)  - (psi_ano(ix,iy,iz)*psi_xx) )
           xV      = ((psi_x(ix,iy,iz) * psi_y(ix,iy,iz)) - (psi_ano(ix,iy,iz) * psi_xy(ix,iy,iz)))

           paramyU = u_clim(ix,iy,iz) / ((a**2) * (cos(lat)))
           paramyV = v_clim(ix,iy,iz) / (a**2)
           yU      = ((psi_x(ix,iy,iz) * psi_y(ix,iy,iz)) - (psi_ano(ix,iy,iz) * psi_xy(ix,iy,iz)))
           yV      = ((psi_y(ix,iy,iz)**2) - (psi_ano(ix,iy,iz) * psi_yy))

           paramzU = u_clim(ix,iy,iz) / (a*cos(lat))
           paramzV = v_clim(ix,iy,iz) / a
           zU      = ((psi_x(ix,iy,iz) * psi_z) - (psi_ano(ix,iy,iz) * psi_xz))
           zV      = ((psi_y(ix,iy,iz) * psi_z) - (psi_ano(ix,iy,iz) * psi_yz))

           parampU = u_clim(ix,iy,iz) / (a*cos(lat))
           parampV = v_clim(ix,iy,iz) / a
           pU      = ((psi_x(ix,iy,iz) * psi_p) - (psi_ano(ix,iy,iz) * psi_xp))
           pV      = ((psi_y(ix,iy,iz) * psi_p) - (psi_ano(ix,iy,iz) * psi_yp))

           termxU = paramxU * xU
           termxV = paramxV * xV

           termyU = paramyU * yU
           termyV = paramyV * yV

           termzU = paramzU * zU
           termzV = paramzV * zV

           termpU = parampU * pU
           termpV = parampV * pV

           wafx(ix,iy,iz) = pcos  * (termxU + termxV)
           wafy(ix,iy,iz) =  pcos * (termyU + termyV)
           wafz(ix,iy,iz) = pcos * zparam * (termzU + termzV)

           wafz_p(ix,iy,iz) = (pcos/p) * pparam * (termpU + termpV)


           u1(ix,iy,iz) = pcos * paramxu * ( psi_x(ix,iy,iz) ** 2)
           u2(ix,iy,iz) = pcos * paramxu * (-1.d0 * (psi_ano(ix,iy,iz) * psi_xx))
           u3(ix,iy,iz) = pcos * paramXV * (psi_x(ix,iy,iz) * psi_y(ix,iy,iz))
           u4(ix,iy,iz) = pcos * paramXV * (-1.d0) * (psi_ano(ix,iy,iz) * psi_xy(ix,iy,iz))

           v1(ix,iy,iz) = pcos * paramYu * (psi_x(ix,iy,iz) * psi_y(ix,iy,iz))
           v2(ix,iy,iz) = pcos * paramYu * (-1.d0) * (psi_ano(ix,iy,iz) * psi_xy(ix,iy,iz))
           v3(ix,iy,iz) = pcos * paramYV * (psi_y(ix,iy,iz) ** 2)
           v4(ix,iy,iz) = pcos * paramYV * (-1.d0) * (psi_ano(ix,iy,iz) * psi_yy)

           z1(ix,iy,iz) = pcos * zparam * paramZu * (psi_x(ix,iy,iz) * psi_z)
           z2(ix,iy,iz) = pcos * zparam * paramZu * (-1.d0) * (psi_ano(ix,iy,iz) * psi_xz)
           z3(ix,iy,iz) = pcos * zparam * paramZV * (psi_y(ix,iy,iz) * psi_z)
           z4(ix,iy,iz) = pcos * zparam * paramZV * (-1.d0) * (psi_ano(ix,iy,iz) * psi_yz)
        enddo
     enddo
  enddo

  do ix=1,defx
     do iz=1,defz
        wafx(ix,1,iz)=undef
        wafy(ix,1,iz)=undef
        wafz(ix,1,iz)=undef
        wafz(ix,1,iz)=undef
        wafx(ix,defy,iz)=undef
        wafy(ix,defy,iz)=undef
        wafz(ix,defy,iz)=undef
     enddo
  enddo
end subroutine calc_waf

end program main
