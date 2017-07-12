!*****************************************************************************
subroutine GauSlaOverlap(expGau,cGau,aGau,expSla,cSla,result)
  implicit none

  BEGIN_DOC
  ! Compute the overlap integral between a Gaussian function
  ! with arbitrary angular momemtum and a s-type Slater function
  END_DOC

! Input variables 
  double precision,intent(in)   :: expGau,expSla
  double precision,intent(in)   :: cGau(3),cSla(3)
  integer,intent(in)            :: aGau(3)
  double precision,intent(out)  :: result

! Final value of the integrals
  double precision              :: ss,ps,ds
  double precision              :: pxs,pys,pzs
  double precision              :: dxxs,dyys,dzzs,dxys,dxzs,dyzs

  double precision              :: pi,E,AB,AxBx,AyBy,AzBz,t,u,k

  pi = 4d0*atan(1d0)
  
! calculate the length AB between the two centers and other usful quantities

  AB = (cGau(1)-cSla(1))**2 + (cGau(2)-cSla(2))**2 + (cGau(3)-cSla(3))**2
  AB = dsqrt(AB)

  AxBx = (cGau(1)-cSla(1))/2d0
  AyBy = (cGau(2)-cSla(2))/2d0
  AzBz = (cGau(3)-cSla(3))/2d0
  ds = 0.d0

! intermediate variables

  t = expSla*dsqrt(0.25d0/expGau)
  u = dsqrt(expGau)*AB

  double precision :: d, et2
  if(AB > 0d0) then 

!   (s|s) 
    ss = 0.d0

    d = derfc(t+u)
    if (dabs(d) > 1.d-30) then
      ss  = (t+u)*d*dexp(2d0*t*(t+u)) 
    endif

    d = derfc(t-u)
    if (dabs(d) > 1.d-30) then
      ss -= (t-u)*d*dexp(2d0*t*(t-u))
    endif

!   (p|s)
    ps = 0.d0
    if (t*t-u*u > 300.d0) then
      et2 = huge(1.0)
    else
      et2 = dexp(t*t-u*u)
    endif
    if (et2 /= 0.d0) then
      d = derfc(t-u) 
      if (d /= 0.d0) then
        ps += dexp((t-u)**2)*(1d0+2d0*t*(t-u))*d
      endif
      d = derfc(t+u)
      if (d /= 0.d0) then
        ps += dexp((t+u)**2)*(1d0+2d0*t*(t+u))*d
      endif
      ps *= dsqrt(pi)
      ps -= 4d0*t 
      ps *= et2/dsqrt(pi)
    endif

!   (d|s)
!    ds = 4d0*dexp(2d0*t*(t-u))*t*(-((1d0+t**2-t*u)*derfc(t-u))+dexp(4d0*t*u)*(1d0+t*(t+u))*derfc(t+u))
    ds = 0.d0
    d = derfc(t+u)
    if (d /= 0.d0) then
      ds  = dexp(4d0*t*u)*(1d0+t*(t+u))*d
    endif
    d = derfc(t-u)
    if (d /= 0.d0) then
      ds -= (1d0+t*t-t*u)*d
    endif

    if ( dabs(ds) > 1.d-100) then
      ds *= 4d0*dexp(2d0*t*(t-u))*t
    endif

!   backward scaling
    ds = 3d0*ss/u**5d0 - 3d0*ps/u**4d0 + ds/u**3d0
    ps = ps/u**2-ss/u**3d0
    ss = ss/u

  else

!   concentric case
    d = derfc(t)
    if (d /= 0.d0) then
      et2 = dexp(t*t)
      ss = 2d0*et2*((-2d0*t)/dsqrt(pi)+et2*(1d0+2d0*t*t)*d)
      ps = (8d0*et2*t*(-2d0*(1d0+t*t)+et2*dsqrt(pi)*t*(3d0+2d0*t*t)*d))/(3d0*dsqrt(pi))
    else
      ss = 0.d0
      ps = 0.d0
    endif

  endif

  k = t**3d0*dexp(-t*t)*4d0*pi/expSla**(3d0/2d0)

! (s|s) 
  ss = k*ss

! (p|s) 
  ps = k*ps

  pxs = AxBx*ps
  pys = AyBy*ps
  pzs = AzBz*ps

! (d|s) 
  ds = k*ds

  dxxs = (2d0*ss+ps)/(4d0*expGau) + AxBx**2*ds
  dyys = (2d0*ss+ps)/(4d0*expGau) + AyBy**2*ds
  dzzs = (2d0*ss+ps)/(4d0*expGau) + AzBz**2*ds

  dxys = AxBx*AyBy*ds
  dxzs = AxBx*AzBz*ds
  dyzs = AyBy*AzBz*ds

  select case (sum(aGau))
    case (0)
      result = ss

    case (1)
      if (aGau(1) == 1) then
        result = pxs
      else if (aGau(2) == 1) then
        result = pys
      else if (aGau(3) == 1) then
        result = pzs
      endif

    case (2)
      if (aGau(1) == 2) then
        result = dxxs
      else if (aGau(2) == 2) then
        result = dyys
      else if (aGau(3) == 2) then
        result = dzzs
      else if (aGau(1)+aGau(2) == 2) then
        result = dxys
      else if (aGau(1)+aGau(3) == 2) then
        result = dxzs
      else if (aGau(2)+aGau(3) == 2) then
        result = dyzs
      endif

    case default
      stop 'GauSlaOverlap not implemented'

  end select

end
!*****************************************************************************

!*****************************************************************************
subroutine GauSlaKinetic(expGau,cGau,aGau,expSla,cSla,result)

  implicit none

  BEGIN_DOC
  ! Compute the kinetic energy integral between a Gaussian function
  ! with arbitrary angular momemtum and a s-type Slater function
  END_DOC

! Input variables 
  double precision,intent(in)   :: expGau,expSla
  double precision,intent(in)   :: cGau(3),cSla(3)
  integer,intent(in)            :: aGau(3)
  double precision,intent(out)  :: result

! Final value of the integrals
  double precision              :: ss,ps,ds
  double precision              :: pxs,pys,pzs
  double precision              :: dxxs,dyys,dzzs,dxys,dxzs,dyzs

  double precision              :: pi,E,AB,AxBx,AyBy,AzBz,t,u,k

  pi = 4d0*atan(1d0)
  
! calculate the length AB between the two centers

  AB = (cGau(1)-cSla(1))**2 + (cGau(2)-cSla(2))**2 + (cGau(3)-cSla(3))**2
  AB = dsqrt(AB)

  AxBx = (cGau(1)-cSla(1))/2d0
  AyBy = (cGau(2)-cSla(2))/2d0
  AzBz = (cGau(3)-cSla(3))/2d0

! intermediate variables

  t = expSla*dsqrt(0.25d0/expGau)
  u = dsqrt(expGau)*AB

  if(AB > 0d0) then 

!   (s|s) 
    ss = (1d0+t*(t-u))*derfc(t-u)*dexp(2d0*t*(t-u)) - (1d0+t*(t+u))*derfc(t+u)*dexp(2d0*t*(t+u))

!   (p|s)
    ps = (dexp(t**2-2d0*t*u-u**2)*(4d0*dexp(2d0*t*u)*(1d0+t**2)    & 
       + dsqrt(pi)*t*(-(dexp(t**2+u**2)*(3d0+2d0*t*(t-u))*derfc(t-u)) & 
       - dexp(2d0*t*u+(t+u)**2)*(3d0+2d0*t*(t+u))*derfc(t+u))))/dsqrt(pi)

!   (d|s)
    ds = (-8d0*dexp(t**2-u**2)*u+4d0*dexp(2d0*t*(t-u))*dsqrt(pi)*t**2*((2d0+t**2-t*u)*derfc(t-u) &
       - dexp(4d0*t*u)*(2d0+t*(t+u))*derfc(t+u)))/dsqrt(pi)

!   backward scaling
    ds = 3d0*ss/u**5d0 - 3d0*ps/u**4d0 + ds/u**3d0
    ps = ps/u**2-ss/u**3d0
    ss = ss/u

  else

!   concentric case
    ss = (4d0*dexp(t**2)*(1d0+t**2))/dsqrt(pi)-2d0*dexp(2d0*t**2)*t*(3d0+2d0*t**2)*derfc(t)
    ps = (8d0*dexp(t**2)*(-1d0+4d0*t**2+2d0*t**4d0-dexp(t**2)*dsqrt(pi)*t**3d0*(5d0+2d0*t**2)*derfc(t)))/(3d0*dsqrt(pi))

  endif

  k = expSla*dsqrt(expGau)*t**3d0*dexp(-t*t)*4d0*pi/expSla**(3d0/2d0)

! (s|s) 
  ss = k*ss

! (p|s) 
  ps = k*ps

  pxs = AxBx*ps
  pys = AyBy*ps
  pzs = AzBz*ps

! (d|s) 
  ds = k*ds

  dxxs = (2d0*ss+ps)/(4d0*expGau) + AxBx**2*ds
  dyys = (2d0*ss+ps)/(4d0*expGau) + AyBy**2*ds
  dzzs = (2d0*ss+ps)/(4d0*expGau) + AzBz**2*ds

  dxys = AxBx*AyBy*ds
  dxzs = AxBx*AzBz*ds
  dyzs = AyBy*AzBz*ds

  select case (sum(aGau))
    case (0)
      result = ss

    case (1)
      if (aGau(1) == 1) then
        result = pxs
      else if (aGau(2) == 1) then
        result = pys
      else if (aGau(3) == 1) then
        result = pzs
      endif

    case (2)
      if (aGau(1) == 2) then
        result = dxxs
      else if (aGau(2) == 2) then
        result = dyys
      else if (aGau(3) == 2) then
        result = dzzs
      else if (aGau(1)+aGau(2) == 2) then
        result = dxys
      else if (aGau(1)+aGau(3) == 2) then
        result = dxzs
      else if (aGau(2)+aGau(3) == 2) then
        result = dyzs
      endif

    case default
      stop 'GauSlaOverlap not implemented'

  end select

end
!*****************************************************************************



!*****************************************************************************
subroutine GauSlaNuclear(expGau,cGau,aGau,expSla,cSla,ZNuc,cNuc,result)

  implicit none

  BEGIN_DOC
  ! Compute the nuclear attraction integral between a Gaussian function
  ! with arbitrary angular momemtum and a s-type Slater function
  END_DOC

! Input variables 
  double precision,intent(in)   :: expGau,expSla
  double precision,intent(in)   :: cGau(3),cSla(3)
  integer,intent(in)            :: aGau(3)
  double precision,intent(in)   :: cNuc(3)
  double precision,intent(in)   :: ZNuc
  double precision,intent(out)  :: result

! Final value of the overlap integral
  double precision              :: ss,ps,ds,fs
  double precision              :: pxs,pys,pzs

  double precision              :: pi,E,AB,x,y,k

  pi = 4d0*atan(1d0)
  E = exp(1d0)
  
! calculate the length AB between the two centers

  AB = (cGau(1)-cSla(1))**2 + (cGau(2)-cSla(2))**2 + (cGau(3)-cSla(3))**2
  AB = dsqrt(AB)

! intermediate variables

  x = dsqrt(expSla**2/(4d0*expGau))
  y = dsqrt(expGau)*AB

  if(AB > 0d0) then 
    ss = (1d0+x*(x+y))*derfc(x+y)*dexp(2d0*x*(x+y)) - (1d0+x*(x-y))*derfc(x-y)*dexp(2d0*x*(x-y))
    ss = ss/y
  else
    ss = (4d0*E**x**2*(1d0+x**2))/dsqrt(Pi)-2d0*E**(2d0*x**2)*x*(3d0+2d0*x**2)*dErfc(x)
  endif

  k = expSla*dsqrt(expGau)*x**3d0*dexp(-x*x)*4d0*pi/expSla**(3d0/2d0)
  ss = k*ss

! Print result
!  write(*,*) ss
  result = 0.d0

end
!*****************************************************************************

double precision function BoysF0(t)
  implicit none
  double precision, intent(in)  :: t
  double precision              :: pi

  pi = 4d0*atan(1d0)

  if(t > 0d0) then
    BoysF0 = 0.5d0*dsqrt(pi/t)*derf(dsqrt(t))
  else
    BoysF0 = 1d0
  endif

end
!*****************************************************************************

!TODO
subroutine GauSlaOverlap_write(expGau,cGau,aGau,expSla,cSla,result,iunit)
  implicit none
  double precision,intent(in)   :: expGau,expSla
  double precision,intent(in)   :: cGau(3),cSla(3)
  integer,intent(in)            :: aGau(3)
  integer,intent(in)            :: iunit
  double precision,intent(out)  :: result
  write(iunit, *)  &
  'SDrSla[ {',expGau,',{',cGau(1),',',cGau(2),',',cGau(3),'},{',aGau(1),',',aGau(2),',',aGau(3),'} },{', expSla,', {',cSla(1),',',cSla(2),',',cSla(3),'} } ],'
  result = 0.d0
end

subroutine GauSlaOverlap_read(expGau,cGau,aGau,expSla,cSla,result,iunit)
  implicit none
  double precision,intent(in)   :: expGau,expSla
  double precision,intent(in)   :: cGau(3),cSla(3)
  integer,intent(in)            :: aGau(3)
  integer,intent(in)            :: iunit
  double precision,intent(out)  :: result
  read(iunit, *)  result
end

subroutine GauSlaKinetic_write(expGau,cGau,aGau,expSla,cSla,result,iunit)
  implicit none
  double precision,intent(in)   :: expGau,expSla
  double precision,intent(in)   :: cGau(3),cSla(3)
  integer,intent(in)            :: aGau(3)
  integer,intent(in)            :: iunit
  double precision,intent(out)  :: result
  write(iunit, *)  &
  'TDrSla[ {',expGau,',{',cGau(1),',',cGau(2),',',cGau(3),'},{',aGau(1),',',aGau(2),',',aGau(3),'} },{', expSla,',{',cSla(1),',',cSla(2),',',cSla(3),'} } ],'
  result = 0.d0
end

subroutine GauSlaKinetic_read(expGau,cGau,aGau,expSla,cSla,result,iunit)
  implicit none
  double precision,intent(in)   :: expGau,expSla
  double precision,intent(in)   :: cGau(3),cSla(3)
  integer,intent(in)            :: aGau(3)
  integer,intent(in)            :: iunit
  double precision,intent(out)  :: result
  read(iunit, *)  result
end

subroutine GauSlaNuclear_write(expGau,cGau,aGau,expSla,cSla,ZNuc,cNuc,result,iunit)
  implicit none
  double precision,intent(in)   :: expGau,expSla
  double precision,intent(in)   :: cGau(3),cSla(3)
  integer,intent(in)            :: aGau(3)
  double precision,intent(in)   :: cNuc(3)
  double precision,intent(in)   :: ZNuc
  integer,intent(in)            :: iunit
  double precision,intent(out)  :: result
  write(iunit, *)  &
  'VDrSla[ {',expGau,',{',cGau(1),',',cGau(2),',',cGau(3),'},{',aGau(1),',',aGau(2),',',aGau(3),'} },{ ', expSla,',{',cSla(1),',',cSla(2),',',cSla(3),'} }, {', ZNuc, ',{', cNuc(1),',', cNuc(2),',', cNuc(3), '} } ],'
  result = 0.d0
end

subroutine GauSlaNuclear_read(expGau,cGau,aGau,expSla,cSla,ZNuc,cNuc,result,iunit)
  implicit none
  double precision,intent(in)   :: expGau,expSla
  double precision,intent(in)   :: cGau(3),cSla(3)
  integer,intent(in)            :: aGau(3)
  double precision,intent(in)   :: cNuc(3)
  double precision,intent(in)   :: ZNuc
  integer,intent(in)            :: iunit
  double precision,intent(out)  :: result
  read(iunit, *)  result
end
!TODO

BEGIN_TEMPLATE

BEGIN_PROVIDER [ double precision, GauSla$X_matrix, (ao_num, nucl_num) ]
  implicit none
  BEGIN_DOC
  ! <Gaussian | Slater> overlap matrix
  END_DOC
  integer                        :: i,j,k
  double precision               :: cGau(3)
  double precision               :: cSla(3)
  double precision               :: expSla, res, expGau
  integer                        :: aGau(3)

!TODO
!  logical :: read
!  integer :: iunit
!  integer :: getunitandopen
!
!  inquire(FILE=trim(ezfio_filename)//'/work/GauSla$X.dat',EXIST=read)
!  if (read) then
!    print *,  'READ $X'
!    iunit = getunitandopen(trim(ezfio_filename)//'/work/GauSla$X.dat','r')
!  else
!    print *,  'WRITE $X'
!    iunit = getunitandopen(trim(ezfio_filename)//'/work/GauSla$X.inp','w')
!    write(iunit,*) '{'
!  endif
!TODO

  do k=1,nucl_num
    cSla(1:3) = nucl_coord_transp(1:3,k)
    expSla    = slater_expo(k)

    do i=1,ao_num
      cGau(1:3) = nucl_coord_transp(1:3, ao_nucl(i))
      aGau(1:3) = ao_power(i,1:3)
      GauSla$X_matrix(i,k) = 0.d0

      do j=1,ao_prim_num(i)
        expGau = ao_expo_ordered_transp(j,i)
        call GauSla$X(expGau,cGau,aGau,expSla,cSla,res)
!        if (read) then
!          call GauSla$X_read(expGau,cGau,aGau,expSla,cSla,res,iunit)
!        else
!          call GauSla$X_write(expGau,cGau,aGau,expSla,cSla,res,iunit)
!        endif
        GauSla$X_matrix(i,k) += ao_coef_normalized_ordered_transp(j,i) * res
      enddo

    enddo

  enddo
!  if (.not.read) then
!    write(iunit,*) '0.}'
!  endif
!  close(iunit)
  
END_PROVIDER

BEGIN_PROVIDER [ double precision, MOSla$X_matrix, (mo_tot_num, nucl_num) ]
  implicit none
  BEGIN_DOC
! <MO | Slater>
  END_DOC
  call dgemm('T','N',mo_tot_num,nucl_num,ao_num,1.d0,                &
      mo_coef, size(mo_coef,1),                        &
      GauSla$X_matrix, size(GauSla$X_matrix,1),                    &
      0.d0, MOSla$X_matrix, size(MOSla$X_matrix,1))
END_PROVIDER

BEGIN_PROVIDER [ double precision, AO_orthoSla$X_matrix, (ao_num, nucl_num) ]
 implicit none
 BEGIN_DOC
 ! <AO_ortho | Slater>
 END_DOC
  call dgemm('T','N',ao_num,nucl_num,ao_num,1.d0,                &
      ao_ortho_canonical_coef, size(ao_ortho_canonical_coef,1),      &
      GauSla$X_matrix, size(GauSla$X_matrix,1),                      &
      0.d0, AO_orthoSla$X_matrix, size(AO_orthoSla$X_matrix,1))
 
END_PROVIDER


SUBST [ X ]

Overlap ;;
Kinetic ;;

END_TEMPLATE

BEGIN_PROVIDER [ double precision, GauSlaNuclear_matrix, (ao_num, nucl_num) ]
  implicit none
  BEGIN_DOC
  ! <Gaussian | Slater> overlap matrix
  END_DOC
  integer                        :: i,j,k,A
  double precision               :: cGau(3)
  double precision               :: cSla(3)
  double precision               :: expSla, res, expGau, Znuc, cNuc(3)
  integer                        :: aGau(3)

!TODO
  logical :: read
  integer :: iunit
  integer :: getunitandopen

  inquire(FILE=trim(ezfio_filename)//'/work/GauSlaNuclear.dat',EXIST=read)
  if (read) then
    print *,  'READ Nuclear'
    iunit = getunitandopen(trim(ezfio_filename)//'/work/GauSlaNuclear.dat','r')
  else
    print *,  'WRITE Nuclear'
    iunit = getunitandopen(trim(ezfio_filename)//'/work/GauSlaNuclear.inp','w')
    write(iunit,*)'{'
  endif
!TODO

  do k=1,nucl_num
    cSla(1:3) = nucl_coord_transp(1:3,k)
    expSla    = slater_expo(k)

    do i=1,ao_num
      cGau(1:3) = nucl_coord_transp(1:3, ao_nucl(i))
      aGau(1:3) = ao_power(i,1:3)
      GauSlaNuclear_matrix(i,k) = 0.d0

      do j=1,ao_prim_num(i)
        expGau = ao_expo_ordered_transp(j,i)
        do A=1,nucl_num
          cNuc(1:3) = nucl_coord_transp(1:3,A)
          Znuc = nucl_charge(A)
!          call GauSlaNuclear(expGau,cGau,aGau,expSla,cSla,Znuc,cNuc,res)
          if (read) then
            call GauSlaNuclear_read(expGau,cGau,aGau,expSla,cSla,Znuc,cNuc,res,iunit)
          else
            call GauSlaNuclear_write(expGau,cGau,aGau,expSla,cSla,Znuc,cNuc,res,iunit)
          endif
          GauSlaNuclear_matrix(i,k) += ao_coef_normalized_ordered_transp(j,i) * res
        enddo
      enddo

    enddo

  enddo
  if (.not.read) then
    write(iunit,*) '0.}'
  endif
  close(iunit)
  
END_PROVIDER

BEGIN_PROVIDER [ double precision, GauSlaH_matrix, (ao_num, nucl_num) ]
 implicit none
 BEGIN_DOC
 ! Core hamiltonian in AO basis
 END_DOC
 GauSlaH_matrix(1:ao_num,1:nucl_num) =                               &
     GauSlaKinetic_matrix(1:ao_num,1:nucl_num) +                     &
     GauSlaNuclear_matrix(1:ao_num,1:nucl_num)

END_PROVIDER


BEGIN_PROVIDER [ double precision, MOSlaNuclear_matrix, (mo_tot_num, nucl_num) ]
  implicit none
  BEGIN_DOC
! <MO | Slater>
  END_DOC
  call dgemm('N','N',mo_tot_num,nucl_num,ao_num,1.d0,                &
      mo_coef_transp, size(mo_coef_transp,1),                        &
      GauSlaNuclear_matrix, size(GauSlaNuclear_matrix,1),                    &
      0.d0, MOSlaNuclear_matrix, size(MOSlaNuclear_matrix,1))
END_PROVIDER

BEGIN_PROVIDER [ double precision, AO_orthoSlaH_matrix, (ao_num, nucl_num) ]
  implicit none
  BEGIN_DOC
! <AO ortho | Slater>
  END_DOC
  call dgemm('T','N',ao_num,nucl_num,ao_num,1.d0,                   &
           ao_ortho_canonical_coef, size(ao_ortho_canonical_coef,1), &
           GauSlaH_matrix, size(GauSlaH_matrix,1),                   &
           0.d0, AO_orthoSlaH_matrix, size(AO_orthoSlaH_matrix,1))
END_PROVIDER

