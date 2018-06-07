

subroutine diffusefraction(radG,altitude,Kt,Ta,RH,radI,radD)
    implicit none 

    real(kind(1d0))                 :: radG,altitude,Kt,Ta,RH,radD,radI,alfa
    REAL(KIND(1D0)),PARAMETER       :: DEG2RAD=0.017453292,RAD2DEG=57.29577951  
 
    alfa=altitude*DEG2RAD
	
    if (Ta<=-99 .or. RH<=-99) then 
        if (Kt<=0.3) then 
            radD=radG*(1.020-0.248*Kt)
        else if (Kt>0.3 .and. Kt<0.78) then 
            radD=radG*(1.45-1.67*Kt)
        else if (Kt>=0.78) then 
            radD=radG*0.147
        end if 
    else
        
        if (Kt<=0.3) then 
            radD=radG*(1-0.232*Kt+0.0239*sin(alfa)-0.000682*Ta+0.0195*(RH/100))
        else if (Kt>0.3 .and. Kt<0.78) then 
            radD=radG*(1.329-1.716*Kt+0.267*sin(alfa)-0.00357*Ta+0.106*(RH/100))
        else if (Kt>=0.78) then 
            radD=radG*(0.426*Kt-0.256*sin(alfa)+0.00349*Ta+0.0734*(RH/100))
        end if 
    end if 
    radI=(radG-radD)/(sin(alfa))

    
    if (radI<0) then 
    radI=0
    end if 

    if (altitude<1 .and. radI>radG) then 
    radI=radG
    end if 

    if (radD>radG) then 
    radD=radG
    end if 
end subroutine diffusefraction 
