program main
  implicit none

  integer,parameter :: &
       & deft=540,s2day=60*60*24

  real,parameter :: &
       & a=6.37E6, &
       & omega=7.292E-5, g=9.80665,cp=1004,Rd=287, Tc=273.15

  real(4), allocatable :: &
       & u_a (:,:,:), v_a (:,:,:), t_a(:,:,:), &
       & w_a (:,:,:), dia_a (:,:,:) ,&
       & z_a (:,:,:) ,strm(:,:,:), &
       & CK (:,:,:), CK_x (:,:,:), CK_y (:,:,:) ,&
       & CP_x (:,:,:), CP_y (:,:,:) ,&
       & KE (:,:,:),   APE (:,:,:),  KE2APE (:,:,:) ,&
       & CQ (:,:,:), &
       & uu (:,:,:), uv (:,:,:), vv (:,:,:) ,&
       & uuh8 (:,:,:), uvh8 (:,:,:), vvh8 (:,:,:) ,&
       & ut (:,:,:), vt (:,:,:), wt (:,:,:) ,&
       & uth8(:,:,:),vth8 (:,:,:), wth8 (:,:,:) ,&
       & CK_u (:,:,:), CK_v (:,:,:), &
       & CK_uh8 (:,:,:), CK_vh8 (:,:,:), &
       & CP_u (:,:,:), CP_v (:,:,:), CP_w (:,:,:), &
       & CP_uh8 (:,:,:), CP_vh8 (:,:,:), CP_wh8 (:,:,:), &
       & EPx (:,:,:) , EPy (:,:,:) ,&
       & u_clim (:,:,:),  v_clim (:,:,:), t_clim (:,:,:) ,&
       & CP_b (:,:,:) , &
       & dAPEdt (:,:,:), &
       & dKEdt (:,:,:), &
       & static_s(:,:,:), static_s1(:,:,:), static_s2(:,:,:), &
       & rho(:,:,:), pres(:,:), plev (:),logz(:)


  integer (4) , allocatable :: &
       & flg(:,:)


  real (4) ::   &
       & pi, Sp, H, &
       & lat,dx,dy, dlon, dlat,&
       & f, f0, &
       & kappa, pp, dux, dvx,duy, dvy,&
       & dudx, dudy, dvdx, dvdy, &
       & undef,lev, &
       & duux, duvy,duvx, dvvy, duudx, duvdy, duvdx, dvvdy,&
       & dvax, dvadx, duay, duady, dvay, dvady, dtax, dtadx, dtay, dtady, &
       & duuh8x, duuh8dx, duvh8y, duvh8dy, & 
       & duvh8x, duvh8dx, dvvh8y, dvvh8dy, &
       & duth8x, duth8dx, dvth8y, dvth8dy, dwth8z, dwth8dz, &
       & dutx, dvty, dwtz, dutdx, dvtdy, dwtdz, CP_eddy_factor, &
       & dTdx, dTdy, danoTdx, danoTdy,dty,dtx, danotx,danoty, &
       & p, danoptdz, dptdz, dz, dpt, danopt, coeff_CP, coeff_CP_wt

  integer :: ix,iy,iz,it,&
       & k, j, &
       & defx, defy, defz, defz2,&
       &  syr, eyr, dyr, z_lev, surface, &
       &  num,imo,l,&
       & num_p,num_n, &
       & pu, pm, pl, &
       & dlamda, dphi, &
       & bs, bs2, bs3, bs4, bs5,&
       & lag, mocom, le

  character(150) FileName, plevfile

  write(*,*) "******************"

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
  read(*,*) lag
  read(*,*) mocom
  read(*,'(a150)') plevfile

  write(*,*) undef
  write(*,*) 'defx',defx
  write(*,*) 'defy',defy
  write(*,*) 'defz',defz
  write(*,*) 'start year',syr
  write(*,*) 'end year',eyr
  write(*,*) 'end year',dyr
  write(*,*) 'lag',lag
  write(*,*) 'mocom',mocom
  write(*,*) 'plevfile ',  plevfile


  allocate (u_a(defx,defy,defz),&
       & v_a(0:defx+1,defy,defz), &
       & w_a(defx,defy,defz), &
       & t_a (0:defx+1,defy,defz) ,&
       & dia_a (0:defx+1,defy,defz) ,&
       & z_a (0:defx+1,defy,defz) ,&
       & strm (0:defx+1,defy,defz) ,&
       & CK   (defx,defy,defz), CK_x (defx,defy,defz), CK_y (defx,defy,defz) ,&
       & CP_x (defx,defy,defz), CP_y (defx,defy,defz) ,&
       & u_clim (0:defx+1,defy,defz) ,&
       & v_clim (0:defx+1,defy,defz), &
       & t_clim (0:defx+1,defy,defz) ,&
       & CP_b (defx,defy,defz) , &
       & KE (defx,defy,defz) , &
       & CQ (defx,defy,defz) , &
       & APE (defx,defy,defz) ,&
       & KE2APE (defx,defy,defz) ,&
       & uu (0:defx,defy,defz), uv (0:defx,defy,defz), vv (defx,defy,defz), &
       & uuh8 (0:defx,defy,defz), uvh8 (0:defx,defy,defz), vvh8 (defx,defy,defz), &
       & ut (0:defx,defy,defz), vt(defx,defy,defz), wt (defx,defy,defz),&
       & uth8 (0:defx,defy,defz), vth8(defx,defy,defz), wth8 (defx,defy,defz),&
       & CK_u (defx,defy,defz), CK_v(defx,defy,defz),&
       & CK_uh8 (defx,defy,defz), CK_vh8 (defx,defy,defz),&
       & CP_u (defx,defy,defz), CP_v(defx,defy,defz), CP_w(defx,defy,defz), &
       & CP_uh8(defx,defy,defz), CP_vh8(defx,defy,defz), CP_wh8(defx,defy,defz),&
       & dAPEdt (defx,defy,defz) ,&
       & dKEdt (defx,defy,defz) ,&
       & static_s (defx,defy,defz), &
       & rho (defx,defy,defz), &
       & pres(defx,defy), &
       & EPx (defx,defy,defz), &
       & EPy (defx,defy,defz), &
       & static_s1 (defx,defy,defz), &
       & static_s2 (defx,defy,defz), &
       & flg(defx,defy) ,&
       & plev (0:defz+1))


  open(1000, file=plevfile)
  read(1000,*)
  do iz=1,defz
     read(1000,*) plev(iz)
  enddo

  if(plev(1) == 1000)then
     do iz=1, defz
        plev(iz) = plev(iz) * 100.d0
     enddo
  endif

  write(*,*) plev(1:defz)

!
! !!!!!!!!1111    I/O file data     !!!!!!!!!!!!!!!!!!!!11
!
  defz2 = defz
  if(defz == 17)then
     defz2 = 13
  endif

  bs = defx * defy * defz * 4
  write(*,*) bs
  bs2 = defx * defy * defz2 * 4
  bs3= defx * defy * 4
  bs4 = defx * defy * 4 * 17
  bs5 = defx * defy * 4 * 13

  le = 1

  call of_udo(9, bs, le) !z
  call of_udo(10, bs, le) !t
  call of_udo(11, bs, le) !u
  call of_udo(12, bs, le) !v
  call of_udo(13, bs2, le) !omega
  call of_udo(14, bs, le) !Q1
  call of_udo(50, bs3, le) !pres

  call of_udo(21, bs, le) !UUh8
  call of_udo(22, bs, le) !UVh8
  call of_udo(23, bs, le) !VVh8
  call of_udo(24, bs, le) !UTha8
  call of_udo(25, bs, le) !VTh8
  !  call of_udo(26, bs5, le) !omegaTh8

  call of_udu(100,bs,le) !output


  ! input climatology

  read(10,rec=5) (((t_clim(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(11,rec=5) (((u_clim(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(12,rec=5) (((v_clim(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)

  if(t_clim(73,50,4) < 200)then
     do ix=1,defx
        do iy=1,defy
           do iz=1,defz
              t_clim(ix,iy,iz) = t_clim(ix,iy,iz) + Tc
           enddo
        enddo
     enddo
  endif

  read(9,rec=1)   (((z_a(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(10,rec=1)   (((t_a(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(11,rec=1) u_a
  read(12,rec=1) (((v_a(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(13,rec=1) w_a
  read(14,rec=1) (((dia_a(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)


  uu=0.d0; uv=0.d0; vv=0.d0; ut=0.d0; vt=0.d0; wt=0.d0 
  uuh8=0.d0; uvh8=0.d0; vvh8=0.d0; uth8=0.d0; vth8=0.d0; wth8=0.d0 

  read(21,rec=1) (((uuh8(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(22,rec=1) (((uvh8(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(23,rec=1) (((vvh8(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(24,rec=1) (((uth8(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(25,rec=1) (((vth8(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)

  read(50,rec=5) pres

  k=1
  call cal_energy_conversion

  write(100,rec=1)   CK_x
  write(100,rec=2)   CK_y
  write(100,rec=5)   CP_x
  write(100,rec=6)   CP_y
  write(100,rec=9)   KE
  write(100,rec=10)   APE
  write(100,rec=11)   CQ
  write(100,rec=12)   static_s
  write(100,rec=13)   KE2APE
  write(100,rec=14)  EPx
  write(100,rec=15)  EPy
  write(100,rec=16)  CK_uh8
  write(100,rec=17)  CK_vh8
  write(100,rec=18)  CP_uh8
  write(100,rec=19)  CP_vh8
  if(2==1)then
     write(100,rec=16)           CP_wh8
     write(100,rec=17)           CK_u
     write(100,rec=18)           CK_v
     write(100,rec=19)           CP_u
     write(100,rec=20)           CP_v
     write(100,rec=21)           CP_w
  endif
  CK_uh8=undef

!  CP_wh8=undef
  wth8(:,:,:) = 0.d0
!
! ----------  calculate data in negative phase  ----------- !
!
  read(9,rec=1+1)   (((z_a(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(10,rec=1+1)   (((t_a(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(11,rec=1+1)   u_a
  read(12,rec=1+1)  (((v_a(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(13,rec=1+1)    w_a
  read(14,rec=1+1)   (((dia_a(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)

! eddy anomaly
  if(1==2)then
     read(15,rec=1+1) uu
     read(16,rec=1+1) uv
     read(17,rec=1+1) vv
     read(18,rec=1+1) ut
     read(19,rec=1+1) vt
     read(20,rec=1+1) wt
  endif
  read(21,rec=1+1) (((uuh8(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(22,rec=1+1) (((uvh8(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(23,rec=1+1) vvh8
  read(24,rec=1+1) (((uth8(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)
  read(25,rec=1+1) (((vth8(ix,iy,iz),ix=1,defx),iy=1,defy),iz=1,defz)

  if(1==2)then
     read(26,rec=1+1) wth8
  endif

  call cal_energy_conversion


  l=20
  write(100,rec=1+l)           CK_x
  write(100,rec=2+l)           CK_y
  write(100,rec=5+l)           CP_x
  write(100,rec=6+l)           CP_y
  write(100,rec=9+l)           KE
  write(100,rec=10+l)           APE
  write(100,rec=11+l)           CQ
  write(100,rec=12+l)           static_s
  write(100,rec=13+l)           KE2APE
  write(100,rec=14+l)           EPx
  write(100,rec=15+l)           EPy
  write(100,rec=16+l)           CK_uh8
  write(100,rec=17+l)           CK_vh8
  write(100,rec=18+l)           CP_uh8
  write(100,rec=19+l)           CP_vh8
  if(1==2)then
     write(100,rec=16+l)           CP_wh8
     write(100,rec=17+l)           CK_u
     write(100,rec=18+l)           CK_v
     write(100,rec=19+l)           CP_u
     write(100,rec=20+l)           CP_v
     write(100,rec=21+l)           CP_w
  endif
  CK_uh8=undef

  stop


contains
  subroutine cal_energy_conversion

!================================================!
!       scale height, dx, dy , logp              !
!  convert to logz coodinate from p coodinate    !
! ============================================== !

    pi = acos(-1.d0)
    !  H = Rd * (-15.d0+273.2)/g
    dlat = real(180,4) / real((defy - 1), 4)
    dx = (real(360,4) / real(defx, 4) ) * pi / 180.d0
    dy = (real(180,4) / real((defy - 1), 4)) * pi / 180.d0
    f0 = 2.d0 * Omega * sin(43.d0 * pi / 180.d0)
    kappa = Rd / Cp

    write(*,*) "***********************"
    write(*,*) defx, defy,dx, dy
    write(*,*) "***********************"
    dlamda = dx*a
    dphi = dy*a

    do iz = 1, defz
       do iy = 1, defy
          lat = (((real(iy - 1) * dy) - (90.d0 * pi / 180.d0)))
          f = 2.d0 * Omega * sin(lat)
          do ix = 1, defx
             strm(ix,iy,iz) = z_a(ix,iy,iz) * g / f
          enddo
       enddo
    enddo

! ============================================== !
! set to boundary conditions
! ============================================== !
    do iy=1,defy
       do iz=1,defz
          t_clim(0,iy,iz)=t_clim(defx,iy,iz)
          t_clim(defx+1,iy,iz)=t_clim(1,iy,iz)
          u_clim(0,iy,iz)=u_clim(defx,iy,iz)
          u_clim(defx+1,iy,iz)=u_clim(1,iy,iz)
          v_clim(0,iy,iz)=v_clim(defx,iy,iz)
          v_clim(defx+1,iy,iz)=v_clim(1,iy,iz)
          v_a(0,iy,iz)=v_a(defx,iy,iz)
          v_a(defx+1,iy,iz)=v_a(1,iy,iz)
          t_a(0,iy,iz)=t_a(defx,iy,iz)
          t_a(defx+1,iy,iz)=t_a(1,iy,iz)
          uu(0,iy,iz)=uu(defx,iy,iz)
          uu(defx+1,iy,iz)=uu(1,iy,iz)
          uuh8(0,iy,iz)=uuh8(defx,iy,iz)
          uuh8(defx+1,iy,iz)=uuh8(1,iy,iz)
          uv(0,iy,iz)=uv(defx,iy,iz)
          uv(defx+1,iy,iz)=uv(1,iy,iz) 
          uvh8(0,iy,iz)=uvh8(defx,iy,iz)
          uvh8(defx+1,iy,iz)=uvh8(1,iy,iz) 
          ut(0,iy,iz)=ut(defx,iy,iz)
          ut(defx+1,iy,iz)=ut(1,iy,iz)
          uth8(0,iy,iz)=uth8(defx,iy,iz)
          uth8(defx+1,iy,iz)=uth8(1,iy,iz)
       enddo
    enddo



! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
! Calculate wave activity flux. This is a main part.
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    flg(:,:) = 0.d0

    write(*,*) plev(1:17)

    do ix=1,defx
       do iy=1,defy
          do iz=1,defz
             lat = ((real(iy-1.d0,4)*dlat)-90.d0)*pi/180.
             dlon=(2.*dlamda*cos(lat))

             dux = u_clim(ix+1,iy,iz) - u_clim(ix-1,iy,iz)
             dudx = dux/dlon
             dvx = v_clim(ix+1,iy,iz) - v_clim(ix-1,iy,iz)
             dvdx = dvx/dlon
             dtx  = (t_clim(ix+1,iy,iz) - t_clim(ix-1,iy,iz) )
             dTdx = dtx / dlon
             dvax = (v_a(ix+1,iy,iz) - v_a(ix-1,iy,iz) )
             dvadx = dvax/dlon
             dtax = (t_a(ix+1,iy,iz) - t_a(ix-1,iy,iz) )
             dtadx = dtax/dlon

             if(iy==1)then
                dty = (t_clim(ix,iy+1,iz) - t_clim(ix,iy,iz))
                dtdy = dty / dphi
                duy = u_clim(ix,iy+1,iz) - u_clim(ix,iy,iz)
                dudy = duy / dphi
                dvy = v_clim(ix,iy+1,iz) - v_clim(ix,iy,iz)
                dvdy = dvy / dphi
                dvty = vt(ix,iy+1,iz) - vt(ix,iy,iz)
                dvtdy = dvty / dphi
                dvth8y = vth8(ix,iy+1,iz) - vth8(ix,iy,iz)
                dvth8dy = dvth8y / dphi
                duvy = uv(ix,iy+1,iz) - uv(ix,iy,iz)
                duvdy = duvy / dphi
                duvh8y = uvh8(ix,iy+1,iz) - uvh8(ix,iy,iz)
                duvh8dy = duvh8y / dphi
                dvvy = vv(ix,iy+1,iz) - vv(ix,iy,iz)
                dvvdy = dvvy / dphi
                dvvh8y = vvh8(ix,iy+1,iz) - vvh8(ix,iy,iz)
                dvvh8dy = dvvh8y / dphi
                duay = u_a(ix,iy+1,iz) - u_a(ix,iy,iz)
                duady = duay / dphi
                dvay = v_a(ix,iy+1,iz) - v_a(ix,iy,iz)
                dvady = dvay / dphi
                dtay = t_a(ix,iy+1,iz) - t_a(ix,iy,iz)
                dtady = dtay / dphi
             elseif(iy == defy)then
                dty = (t_clim(ix,iy,iz) - t_clim(ix,iy-1,iz))
                dtdy = dty / dphi
                duy = u_clim(ix,iy,iz) - u_clim(ix,iy-1,iz)
                dudy = duy / dphi
                dvy = v_clim(ix,iy,iz) - v_clim(ix,iy-1,iz)
                dvdy = dvy / dphi
                dvty = vt(ix,iy,iz) - vt(ix,iy-1,iz)
                dvtdy = dvty / dphi
                dvth8y = vth8(ix,iy,iz) - vth8(ix,iy-1,iz)
                dvth8dy = dvth8y / dphi
                duvy = uv(ix,iy,iz) - uv(ix,iy-1,iz)
                duvdy = duvy / dphi
                duvh8y = uvh8(ix,iy,iz) - uvh8(ix,iy-1,iz)
                duvh8dy = duvh8y / dphi
                dvvy = vv(ix,iy,iz) - vv(ix,iy-1,iz)
                dvvdy = dvvy / dphi
                dvvh8y = vvh8(ix,iy,iz) - vvh8(ix,iy-1,iz)
                dvvh8dy = dvvh8y / dphi
                duay = u_a(ix,iy,iz) - u_a(ix,iy-1,iz)
                duady = duay / dphi
                dvay = v_a(ix,iy,iz) - v_a(ix,iy-1,iz)
                dvady = dvay / dphi
                dtay = t_a(ix,iy,iz) - t_a(ix,iy-1,iz)
                dtady = dtay / dphi
             else
                duy = u_clim(ix,iy+1,iz) - u_clim(ix,iy-1,iz)
                dudy = duy / (2.d0*dphi)
                dvy = v_clim(ix,iy+1,iz) - v_clim(ix,iy-1,iz)
                dvdy = dvy / (2.d0*dphi)
                dTy = (t_clim(ix,iy+1,iz) - t_clim(ix,iy-1,iz))
                dTdy = dty / (2.d0*dphi)
                dvty = vt(ix,iy+1,iz) - vt(ix,iy-1,iz)
                dvtdy = dvty / (2.d0*dphi)
                dvth8y = vth8(ix,iy+1,iz) - vth8(ix,iy-1,iz)
                dvth8dy = dvth8y / (2.d0*dphi)
                duvy = uv(ix,iy+1,iz) - uv(ix,iy-1,iz)
                duvdy = duvy / (2.d0*dphi)
                duvh8y = uvh8(ix,iy+1,iz) - uvh8(ix,iy-1,iz)
                duvh8dy = duvh8y / (2.d0*dphi)
                dvvy = vv(ix,iy+1,iz) - vv(ix,iy-1,iz)
                dvvdy = dvvy / (2.d0*dphi)
                dvvh8y = vvh8(ix,iy+1,iz) - vvh8(ix,iy-1,iz)
                dvvh8dy = dvvh8y / (2.d0*dphi)
                duay = u_a(ix,iy+1,iz) - u_a(ix,iy-1,iz)
                duady = duay / (2.d0*dphi)
                dvay = v_a(ix,iy+1,iz) - v_a(ix,iy-1,iz)
                dvady = dvay / (2.d0*dphi)
                dtay = t_a(ix,iy+1,iz) - t_a(ix,iy-1,iz)
                dtady = dtay / (2.d0*dphi)
             endif

             if(flg(ix,iy) == 1)then
                ! top level of atmosphere
                if(iz==defz)then
                   Sp = (Rd*t_clim(ix,iy,iz) / (Cp*plev(iz)))  - &
                        & ((t_clim(ix,iy,iz) - t_clim(ix,iy,iz-1)) / (plev(iz) - plev(iz-1)))
                   static_s1(ix,iy,iz) = (Rd * t_clim(ix,iy,iz) / (Cp*plev(iz)))
                   static_s2(ix,iy,iz) = ((t_clim(ix,iy,iz) - t_clim(ix,iy,iz-1)) / (plev(iz) - plev(iz-1)))

                   dwtz = wt(ix,iy,iz) - wt(ix,iy,iz-1)
                   dwth8z = wth8(ix,iy,iz) - wth8(ix,iy,iz-1)
                   dz = plev(iz) - plev(iz-1)
                else
                   Sp = (Rd*t_clim(ix,iy,iz) / (Cp * plev(iz)))  - &
                        & ((t_clim(ix,iy,iz+1) - t_clim(ix,iy,iz-1)) / (plev(iz+1) - plev(iz-1)))
                   static_s1(ix,iy,iz) = (Rd*t_clim(ix,iy,iz) / (Cp*plev(iz)))
                   static_s2(ix,iy,iz) =  ((t_clim(ix,iy,iz+1) - t_clim(ix,iy,iz-1)) / (plev(iz+1) - plev(iz-1)))

                   dwtz = wt(ix,iy,iz+1) - wt(ix,iy,iz-1)
                   dwth8z = wth8(ix,iy,iz+1) - wth8(ix,iy,iz-1)
                   dz = plev(iz+1)-plev(iz-1)
                endif

                !flg=0 means surface or no-realistic space (below surface pressure, e.g. Mountain).
             elseif(flg(ix,iy) == 0.d0)then
                ! bottomo level of atmosphere (surface pressure)
                ! if(plev(iz) > (pres(ix,iy)*100.d0))then

                if((plev(iz) > (pres(ix,iy))) .or. t_clim(ix,iy,iz) == undef )then
                   Sp = undef

                   dwtz = undef
                   dz = undef
                else
                   Sp = (Rd*t_clim(ix,iy,iz) / (Cp*plev(iz))) - &
                        & ((t_clim(ix,iy,iz+1) - t_clim(ix,iy,iz)) / (plev(iz+1) - plev(iz)))

                   static_s1(ix,iy,iz) = (Rd*t_clim(ix,iy,iz) / (Cp*plev(iz)))
                   static_s2(ix,iy,iz) = ((t_clim(ix,iy,iz+1) - t_clim(ix,iy,iz)) /(plev(iz+1) - plev(iz)))

                   dwtz = wt(ix,iy,iz+1) - wt(ix,iy,iz)
                   dwth8z = wth8(ix,iy,iz+1) - wth8(ix,iy,iz)
                   dz = plev(iz+1) - plev(iz)

                   flg(ix,iy) = 1.d0
                endif
             endif

             static_s(ix,iy,iz) = Sp

             EPx(ix,iy,iz) = (v_a(ix,iy,iz)**2 - u_a(ix,iy,iz) **2)
             CK_x(ix,iy,iz) = (EPx(ix,iy,iz) / 2) * (dudx - dvdy)
             EPy(ix,iy,iz) = (-1.d0) * u_a(ix,iy,iz) * v_a(ix,iy,iz)
             CK_y(ix,iy,iz) = EPy(ix,iy,iz) * (dudy + dvdx)
             CK(ix,iy,iz) = CK_x(ix,iy,iz) + CK_y(ix,iy,iz)


             coeff_CP = ((-1.d0) * Rd / (Sp * plev(iz)))
             CP_x (ix,iy,iz) = coeff_CP * ((u_a(ix,iy,iz) * t_a(ix,iy,iz) * dtdx))
             CP_y (ix,iy,iz) = coeff_CP * ((v_a(ix,iy,iz) * t_a(ix,iy,iz) * dtdy))
             CP_b(ix,iy,iz) = CP_x(ix,iy,iz) + CP_y(ix,iy,iz)

             CQ(ix,iy,iz) = (Rd * t_a(ix,iy,iz) *dia_a(ix,iy,iz)/(Sp*plev(iz)))/86400

             !
             ! calcilate CP, CK by eddy momentum, heat flux convergence
             !

             dutx = ut(ix+1,iy,iz) - ut(ix-1,iy,iz)
             dutdx = dutx/dlon
             duth8x = uth8(ix+1,iy,iz) - uth8(ix-1,iy,iz)
             duth8dx = duth8x/dlon

             duux = uu(ix+1,iy,iz) - uu(ix-1,iy,iz)
             duudx = duux/dlon
             duuh8x = uuh8(ix+1,iy,iz) - uuh8(ix-1,iy,iz)
             duuh8dx = duuh8x/dlon
             duvx = uv(ix+1,iy,iz) - uv(ix-1,iy,iz)
             duvdx = duvx/dlon
             duvh8x = uvh8(ix+1,iy,iz) - uvh8(ix-1,iy,iz)
             duvh8dx = duvh8x/dlon
             CK_u(ix,iy,iz) = -1.d0 * u_a(ix,iy,iz) * (duudx + duvdy)
             CK_uh8(ix,iy,iz) = -1.d0 * u_a(ix,iy,iz) * (duuh8dx + duvh8dy)
             CK_v(ix,iy,iz) = -1.d0 * v_a(ix,iy,iz) * (duvdx + dvvdy)
             CK_vh8(ix,iy,iz) = -1.d0 * v_a(ix,iy,iz) * (duvh8dx + dvvh8dy)

             CP_u(ix,iy,iz) = coeff_CP * t_a(ix,iy,iz)*dutdx
             CP_v(ix,iy,iz) = coeff_CP * t_a(ix,iy,iz)*dvtdy

             CP_uh8(ix,iy,iz) = coeff_CP * t_a(ix,iy,iz) * duth8dx
             CP_vh8(ix,iy,iz) = coeff_CP * t_a(ix,iy,iz) * dvth8dy

             coeff_CP_wt = (plev(iz) / plev(1)) ** kappa
             dwtdz = dwtz / dz
             dwth8dz = dwth8z / dz

             CP_w(ix,iy,iz) = coeff_CP_wt * coeff_CP * t_a(ix,iy,iz) * dwtdz
             CP_wh8(ix,iy,iz) = coeff_CP_wt * coeff_CP * t_a(ix,iy,iz) * dwth8dz

             !                                       !
             ! calculate KE & APE, local density  ** !
             !                                       !
             KE(ix,iy,iz) = ((u_a(ix,iy,iz) **2)  + (v_a(ix,iy,iz) **2))/2
             APE(ix,iy,iz) = (Rd * (t_a(ix,iy,iz)**2)/plev(iz)) / (2 * Sp)
             KE2APE(ix,iy,iz) = w_a(ix,iy,iz) * t_a(ix,iy,iz) * Rd / plev(iz)
             rho(ix,iy,iz) = plev(iz) / (Rd * ( t_clim(ix,iy,iz) + t_a(iz,iy,iz) ) )

             dAPEdt(ix,iy,iz) = (cp_b(ix,iy,iz) + cq(ix,iy,iz) + ke2APE(ix,iy,iz))
             dKEdt(ix,iy,iz) = (CK(ix,iy,iz) - ke2APE(ix,iy,iz))

             if(Sp <= 0.0)then
                APE(ix,iy,iz) = undef
                CQ(ix,iy,iz) = undef
                CP_b(ix,iy,iz) = undef
                dAPEdt(ix,iy,iz) = undef
                CP_u(ix,iy,iz) = undef
                CP_v(ix,iy,iz) = undef
                CP_vh8(ix,iy,iz) = undef
                CP_w(ix,iy,iz) = undef
                CP_wh8(ix,iy,iz) = undef
             endif


             if(iy == 1 .or. iy == defy)then
                CK_x(ix,iy,iz) = undef; CK_y(ix,iy,iz) = undef
                CP_x(ix,iy,iz) = undef; CP_y(ix,iy,iz) = undef
                CP_b(ix,iy,iz) = undef
                CK(ix,iy,iz) = undef
                dAPEdt(ix,iy,iz) = undef
                CP_u(ix,iy,iz) = undef; CP_v(ix,iy,iz) = undef
                CP_w(ix,iy,iz) = undef
                CP_uh8(ix,iy,iz) = undef; CP_vh8(ix,iy,iz) = undef; CP_wh8(ix,iy,iz) = undef
                CK_u(ix,iy,iz) = undef; CK_v(ix,iy,iz) = undef
                CK_uh8(ix,iy,iz) = undef; CK_vh8(ix,iy,iz) = undef
             endif

             if(static_s(ix,iy,iz) == undef)then
                CP_x(ix,iy,iz) = undef; CP_y(ix,iy,iz) = undef
                CQ(ix,iy,iz) = undef
                APE(ix,iy,iz) = undef
                CP_b(ix,iy,iz) = undef
                dAPEdt(ix,iy,iz) = undef

                CP_u(ix,iy,iz) = undef; CP_v(ix,iy,iz) = undef; CP_w(ix,iy,iz) = undef
                CP_uh8(ix,iy,iz) = undef; CP_vh8(ix,iy,iz) = undef; CP_wh8(ix,iy,iz) = undef
                CK_u(ix,iy,iz) = undef; CK_v(ix,iy,iz) = undef
                CK_uh8(ix,iy,iz) = undef; CK_vh8(ix,iy,iz) = undef
             endif

             if(flg(ix,iy) == 0)then
                static_s(ix,iy,iz) = undef
                CP_u(ix,iy,iz) = undef; CP_v(ix,iy,iz) = undef; CP_w(ix,iy,iz) = undef
                CP_vh8(ix,iy,iz) = undef; CP_wh8(ix,iy,iz) =undef
             endif

             if (static_s(ix,iy,iz) <= 1.0E-5)then
                CP_b(ix,iy,iz) = undef
                CP_x(ix,iy,iz) = undef; CP_y(ix,iy,iz) = undef
                CQ(ix,iy,iz) = undef
                dAPEdt(ix,iy,iz) =undef

                CP_u(ix,iy,iz) = undef; CP_v(ix,iy,iz) = undef; CP_w(ix,iy,iz) = undef
                CP_uh8(ix,iy,iz) = undef; CP_vh8(ix,iy,iz) = undef; CP_wh8(ix,iy,iz) = undef
             endif

             if(iz == 1 .or. iz >= defz2)then
                !  CP_wh8(ix,iy,iz) = undef
             endif

             !If anomalous data is put on undef,
             if(u_clim(ix,iy,iz) == undef .or. u_a(ix,iy,iz) == undef &
                  & .or. v_a(ix,iy,iz) == undef .or. v_clim(ix,iy,iz) == undef )then
                CK_x(ix,iy,iz) = undef; CK_y(ix,iy,iz) = undef
             endif

             if(t_a(ix,iy,iz) == undef .or. u_a(ix,iy,iz) == undef &
                  & .or. v_a(ix,iy,iz) == undef .or. t_clim(ix,iy,iz) == undef )then
                CP_x(ix,iy,iz) = undef; CP_y(ix,iy,iz) = undef
             endif

          enddo
       enddo
    enddo


    return

    ! output for anomaly data.

    stop
  endsubroutine cal_energy_conversion

endprogram main
