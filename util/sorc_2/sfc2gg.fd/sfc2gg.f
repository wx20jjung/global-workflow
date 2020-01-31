program sfc2gg
  use sfcio_module
! Program History Log:
!
! May 2013 - Shrinivas Moorthi - Updated for WCOSS - to use new bacio library
!                                 and call to wryte
!
  implicit none
  integer narg,iargc
  integer(sfcio_intkind),parameter:: lusfc=11,luggg=51,luctl=52
  integer(sfcio_intkind):: irets
  character(255) cfsfc,cfggg,cfctl,cidrt
  integer ncfsfc,ncfggg,ncfctl
  integer iret,nsfc,n,idrt
  type(sfcio_head),allocatable:: head(:)
  type(sfcio_data):: data
  narg=iargc()
  if(narg.lt.4) then
     if(narg.ne.0) call errmsg('sfc2gg: at least 4 arguments required')
     call eusage
     call errexit(1)
  endif
  call getarg(narg-2,cfggg)
  ncfggg=len_trim(cfggg)
! open(luggg,file=cfggg(1:ncfggg),action='write',form='unformatted',iostat=iret)
  call baopenwt(luggg,cfggg(1:ncfggg),iret)
  if(iret.ne.0) then
     call errmsg('sfc2gg: error opening file '//cfggg(1:ncfggg))
     call errexit(2)
  endif
  call getarg(narg-1,cfctl)
  ncfctl=len_trim(cfctl)
  open(luctl,file=cfctl(1:ncfctl),status='replace',iostat=iret)
  if(iret.ne.0) then
     call errmsg('sfc2gg: error opening file '//cfctl(1:ncfctl))
     call errexit(2)
  endif
  call getarg(narg,cidrt)
  n=len_trim(cidrt)
  if (cidrt(1:n) .eq. '0' ) then
    idrt = 0
  else
    idrt = 4
  endif
  nsfc=narg-3
  allocate(head(nsfc))
  do n=1,nsfc
    call getarg(n,cfsfc)
    ncfsfc=len_trim(cfsfc)
    call sfcio_srohdc(lusfc,cfsfc(1:ncfsfc),head(n),data,irets)
    if(irets.ne.0) then
print *,'irets=',irets
       call errmsg('sfc2gg: error opening file '//cfsfc(1:ncfsfc))
       call errexit(2)
    endif
    if(head(n)%latb.ne.head(1)%latb.or.&
       head(n)%lonb.ne.head(1)%lonb.or.&
       head(n)%lsoil.ne.head(1)%lsoil.or.&
       head(n)%ivs.ne.head(1)%ivs) then
       call errmsg('sfc2gg: incompatible data in file '//cfsfc(1:ncfsfc))
       call errexit(2)
    endif
    call sfc2gg1(luggg,head(n),data)
    call sfcio_axdata(data,irets)
  enddo
  call sfc2gg2(luctl,nsfc,head,cfggg(1:ncfggg),idrt)
contains
subroutine eusage
  implicit none
  call errmsg('Usage: sfc2gg sfcfile(s) gggfile ctlfile idrt')
end subroutine
end program
subroutine sfc2gg1(luggg,head,data)
  use sfcio_module
  implicit none
  integer,         intent(in) :: luggg
  type(sfcio_head),intent(in) :: head
  type(sfcio_data),intent(in) :: data
  integer l, ijm4
  ijm4 = 4 * head%lonb * head%latb
  call wryte(luggg, ijm4, data%tsea)
  do l=1,head%lsoil
    write(luggg) data%smc(:,:,l)
    call wryte(luggg, ijm4, data%smc(:,:,l))
  enddo
  call wryte(luggg, ijm4, data%sheleg)
  do l=1,head%lsoil
    call wryte(luggg, ijm4, data%stc(:,:,l))
  enddo
  call wryte(luggg, ijm4, data%tg3)
  call wryte(luggg, ijm4, data%zorl)
  call wryte(luggg, ijm4, data%cv)
  call wryte(luggg, ijm4, data%cvb)
  call wryte(luggg, ijm4, data%cvt)
  call wryte(luggg, ijm4, data%alvsf)
  call wryte(luggg, ijm4, data%alvwf)
  call wryte(luggg, ijm4, data%alnsf)
  call wryte(luggg, ijm4, data%alnwf)
  call wryte(luggg, ijm4, data%slmsk)
  call wryte(luggg, ijm4, data%vfrac)
  call wryte(luggg, ijm4, data%canopy)
  call wryte(luggg, ijm4, data%f10m)
  call wryte(luggg, ijm4, data%vtype)
  call wryte(luggg, ijm4, data%stype)
  call wryte(luggg, ijm4, data%facsf)
  call wryte(luggg, ijm4, data%facwf)
  call wryte(luggg, ijm4, data%uustar)
  call wryte(luggg, ijm4, data%ffmm)
  call wryte(luggg, ijm4, data%ffhh)
  call wryte(luggg, ijm4, data%hice)
  call wryte(luggg, ijm4, data%fice)
  call wryte(luggg, ijm4, data%tprcp)
  call wryte(luggg, ijm4, data%srflag)
  call wryte(luggg, ijm4, data%snwdph)
  do l=1,head%lsoil
    call wryte(luggg, ijm4, data%slc(:,:,l))
  enddo
  call wryte(luggg, ijm4, data%shdmin)
  call wryte(luggg, ijm4, data%shdmax)
  call wryte(luggg, ijm4, data%slope)
  call wryte(luggg, ijm4, data%snoalb)
  call wryte(luggg, ijm4, data%orog)
  if (head%ivs .ge. 200509) then
    call wryte(luggg, ijm4, data%t2m)
    call wryte(luggg, ijm4, data%q2m)
    call wryte(luggg, ijm4, data%tisfc)
  endif
end subroutine
subroutine sfc2gg2(luctl,nsfc,head,cfggg,idrt)
  use sfcio_module
  implicit none
  integer,intent(in):: luctl,nsfc,idrt
  type(sfcio_head),intent(in):: head(nsfc)
  character*(*) cfggg
  real(4),allocatable:: slat(:),wlat(:)
  integer idat(8),jdat(8),jhr
  real(4) rinc(5)
  character*10 cdat(8)
  integer n
  call w3movdat((/0.,head(1)%fhour,0.,0.,0./),&
                (/head(1)%idate(4),head(1)%idate(2),head(1)%idate(3),0,&
                  head(1)%idate(1),0,0,0/),idat)
  call w3pradat(idat,cdat)
  if(nsfc.gt.1) then
    call w3movdat((/0.,head(2)%fhour,0.,0.,0./),&
                  (/head(2)%idate(4),head(2)%idate(2),head(2)%idate(3),0,&
                    head(2)%idate(1),0,0,0/),jdat)
    call w3difdat(jdat,idat,2,rinc)
    jhr=nint(rinc(2))
  else
    jhr=12
  endif
  if(cfggg(1:1).eq.'/') then
    write(luctl,'("dset ",a)') cfggg
  else
    write(luctl,'("dset ^",a)') cfggg
  endif
  write(luctl,'("options yrev")')
! write(luctl,'("options yrev sequential")')
  write(luctl,'("undef -9.99E+33")')
  write(luctl,'("title sfc2gg")')
  write(luctl,'("xdef",i6," linear",2f12.6)') head(1)%lonb,0.d0,360.d0/head(1)%lonb
  allocate(slat(head(1)%latb),wlat(head(1)%latb))
  call splat(idrt,head(1)%latb,slat,wlat)
  write(luctl,'("ydef",i6," levels")') head(1)%latb
  write(luctl,'(5f12.6)') 180.d0/acos(-1.d0)*asin(dble(slat(head(1)%latb:1:-1)))
  write(luctl,'("zdef",i6," levels")') head(1)%lsoil
  write(luctl,'(5f12.6)') head(1)%zsoil
  write(luctl,'("tdef",i6," linear ",i2.2,"Z",i2.2,a3,i4.4,1x,i6,"hr")')&
   nsfc,idat(5),idat(3),cdat(2)(1:3),idat(1),jhr
  if (head(1)%ivs .ge. 200509) then
    write(luctl,'("vars",i6)') 38
  else
    write(luctl,'("vars",i6)') 35
  endif
  write(luctl,'("tsea    ",i3," 99 surface temperature (K)")') 1
  write(luctl,'("smc     ",i3," 99 soil volumetric water content ()")') head(1)%lsoil
  write(luctl,'("sheleg  ",i3," 99 snow depth (m)")') 1
  write(luctl,'("stc     ",i3," 99 soil temperature (K)")') head(1)%lsoil
  write(luctl,'("tg3     ",i3," 99 deep soil temperature (K)")') 1
  write(luctl,'("zorl    ",i3," 99 roughness (cm)")') 1
  write(luctl,'("cv      ",i3," 99 convective cloud cover ()")') 1
  write(luctl,'("cvb     ",i3," 99 convective cloud bottom (kPa)")') 1
  write(luctl,'("cvt     ",i3," 99 convective cloud top (kPa)")') 1
  write(luctl,'("alvsf   ",i3," 99 albedo for visible scattered ()")') 1
  write(luctl,'("alvwf   ",i3," 99 albedo for visible beam ()")') 1
  write(luctl,'("alnsf   ",i3," 99 albedo for near-IR scattered ()")') 1
  write(luctl,'("alnwf   ",i3," 99 albedo for near-IR beam ()")') 1
  write(luctl,'("slmsk   ",i3," 99 sea-land-ice mask (0-sea, 1-land, 2-ice)")') 1
  write(luctl,'("vfrac   ",i3," 99 vegetation fraction ()")') 1
  write(luctl,'("canopy  ",i3," 99 canopy water (m)")') 1
  write(luctl,'("f10m    ",i3," 99 10-meter wind speed over lowest model wind speed ()")') 1
  write(luctl,'("vtype   ",i3," 99 vegetation type (integer 1-13)")') 1
  write(luctl,'("stype   ",i3," 99 soil type (integer 1-9)")') 1
  write(luctl,'("facsf   ",i3," 99 ???")') 1
  write(luctl,'("facwf   ",i3," 99 ???")') 1
  write(luctl,'("uustar  ",i3," 99 ???")') 1
  write(luctl,'("ffmm    ",i3," 99 ???")') 1
  write(luctl,'("ffhh    ",i3," 99 ???")') 1
  write(luctl,'("hice    ",i3," 99 ???")') 1
  write(luctl,'("fice    ",i3," 99 ???")') 1
  write(luctl,'("tprcp   ",i3," 99 ???")') 1
  write(luctl,'("srflag  ",i3," 99 ???")') 1
  write(luctl,'("snwdph  ",i3," 99 ???")') 1
  write(luctl,'("slc     ",i3," 99 ???")') head(1)%lsoil
  write(luctl,'("shdmin  ",i3," 99 ???")') 1
  write(luctl,'("shdmax  ",i3," 99 ???")') 1
  write(luctl,'("slope   ",i3," 99 ???")') 1
  write(luctl,'("snoalb  ",i3," 99 ???")') 1
  write(luctl,'("orog    ",i3," 99 orography (m)")') 1
  if (head(1)%ivs .ge. 200509) then
    write(luctl,'("t2m    ",i3," 99 t2m (K)")') 1
    write(luctl,'("q2m    ",i3," 99 q2m (kg/kg)")') 1
    write(luctl,'("tisfc    ",i3," 99 Ice temperature (K)")') 1
  endif
  write(luctl,'("endvars")')
end subroutine
