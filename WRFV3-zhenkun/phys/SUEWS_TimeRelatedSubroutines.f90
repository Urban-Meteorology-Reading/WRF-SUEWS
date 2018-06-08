






















 subroutine day2month(b,mb,md,seas,year,latitude)
   IMPLICIT NONE
   integer,intent(in) ::b  
   integer,intent(out) ::mb 
   integer,intent(out) ::md 
   integer,intent(out) ::seas
   integer,intent(in) ::year
   integer::t1,t2,t3
   integer::k 

   real (kind(1d0))::latitude

   

   t1=4
   t2=100
   t3=400

   if ((modulo(year,t1)==0).and.(modulo(year,t2)/=0).or.(modulo(year,t3)==0)) then
      K=1
   else
      K=0
   endif

   IF(B<=31) THEN 
     MB=1
     md=B
   ELSEIF(B>31 .AND. B<=59+K) THEN
     MB=2
     md=B-31
   ELSEIF(B>59+K .AND. B<=90+K) THEN
     MB=3
     md=B-(59+K)
   ELSEIF(B>90+K .AND. B<=120+K) THEN
     MB=4
     md=B-(90+K)
   ELSEIF(B>120+K .AND. B<=151+K) THEN
     MB=5
     md=B-(120+K)
   ELSEIF(B>151+K .AND. B<=181+K) THEN
     MB=6
     md=B-(151+K)
   ELSEIF(B>181+K .AND. B<=212+K) THEN
     MB=7
     md=B-(181+K)
   ELSEIF(B>212+K .AND. B<=243+K) THEN
     MB=8
     md=B-(212+K)
   ELSEIF(B>243+K .AND. B<=273+K) THEN
     MB=9
     md=B-(243+K)
   ELSEIF(B>273+K .AND. B<=304+K)THEN
     MB=10
     md=B-(273+K)
   ELSEIF(B>304+K .AND. B<=334+K) THEN
     MB=11
     md=B-(304+K)
   ELSEIF(B>334+K) THEN
     MB=12
     md=B-(334+K)
   ENDIF

   
   if(latitude>0)then  
       IF (mb>3 .AND. mb<10) THEN 
         seas=1
       else
         seas=2 
       endif
   else  
       IF (mb<4 .or. mb>9) THEN 
         seas=1
       else
         seas=2 
       endif
   endif
   return
 end  subroutine day2month

 subroutine month2day(mon,ne,k,b)
  IMPLICIT NONE
  integer:: mon,ne,k,b

  IF(mon== 1)THEN
    NE=32-B
  ELSE IF(mon==2)THEN
    NE=60+K-B
  ELSE IF(mon==3)THEN
    NE=91+K-B
  ELSE IF(mon==4)THEN
    NE=121+K-B
  ELSE IF(mon==5) THEN
    NE=152+K-B
  ELSE IF(mon==6) THEN
    NE=182+K-B
  ELSE IF(mon==7)THEN
    NE=213+K-B
  ELSE IF(mon==8) THEN
    NE=244+K-B

  ELSE IF(mon==9)THEN
    NE=274+K-B
  ELSE IF(mon==10) THEN
    NE=305+K-B
  ELSE IF(mon==11) THEN
    NE=335+K-B
  ELSE IF(mon==12)THEN
    NE=366+K-B
  END IF
 end subroutine month2day


 subroutine LeapYearCalc(year_int,nroDays)

  IMPLICIT NONE

  integer :: nroDays,year_int

  IF(MOD(year_int,100).NE.0.AND.MOD(year_int,4).EQ.0) THEN
    nroDays=366
  ELSEIF(MOD(year_int,400).EQ.0) THEN
    nroDays=366
  ELSE
    nroDays=365
  ENDIF
 end subroutine LeapYearCalc



 subroutine Day_Of_Week(DATE, MONTH, YEAR, DOW)
 
 
 

 IMPLICIT NONE

 INTEGER DATE, MONTH, DAY, YR, MN, N1, N2, DOW, YEAR

   YR = YEAR
   MN = MONTH




        IF (MN.GT.2)GO TO 10
        MN = MN + 12
        YR = YR - 1
10      N1 = (26 * (MN + 1)) / 10
        N2 = (125 * YR) / 100
        DAY = (DATE + N1 + N2 - (YR / 100) + (YR / 400) - 1)
        DOW = MOD(DAY, 7) + 1

        RETURN
 END subroutine Day_Of_Week




subroutine dectime_to_timevec(dectime,HOURS,MINS,SECS)
    
    
    INTEGER :: HOURS, MINS, doy
    REAL(kind(1d0))    :: dectime,SECS,DH,DM,DS
    

        doy=FLOOR(dectime)

        DH=dectime-doy 
        HOURS=int(24*DH)

        DM=24*DH-HOURS 
        MINS=int(60*DM)

        DS=60*DM-MINS
        SECS=int(60*DS)

end subroutine dectime_to_timevec









SUBROUTINE DAYLEN(DOY, XLAT,DAYL, DEC, SNDN, SNUP)

    IMPLICIT NONE
    INTEGER :: DOY
    REAL(kind(1d0)) :: DEC,DAYL,SOC,SNDN,SNUP,XLAT
    REAL(kind(1d0)),PARAMETER :: PI=3.14159, RAD=PI/180.0




      DEC = -23.45 * COS(2.0*PI*(DOY+10.0)/365.0)


      SOC = TAN(RAD*DEC) * TAN(RAD*XLAT)
      SOC = MIN(MAX(SOC,-1.0),1.0)


      DAYL = 12.0 + 24.0*ASIN(SOC)/PI
      SNUP = 12.0 - DAYL/2.0
      SNDN = 12.0 + DAYL/2.0

END SUBROUTINE DAYLEN
















