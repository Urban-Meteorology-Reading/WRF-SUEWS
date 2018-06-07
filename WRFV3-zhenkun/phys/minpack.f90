subroutine chkder ( m, n, x, fvec, fjac, ldfjac, xp, fvecp, mode, err )


























































































  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) eps
  real ( kind = 8 ) epsf
  real ( kind = 8 ) epslog
  real ( kind = 8 ) epsmch
  real ( kind = 8 ) err(m)
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fvec(m)
  real ( kind = 8 ) fvecp(m)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) mode
  real ( kind = 8 ) temp
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xp(n)

  epsmch = epsilon ( epsmch )
  eps = sqrt ( epsmch )



  if ( mode == 1 ) then

    do j = 1, n
      temp = eps * abs ( x(j) )
      if ( temp == 0.0D+00 ) then
        temp = eps
      end if
      xp(j) = x(j) + temp
    end do



  else if ( mode == 2 ) then

    epsf = 100.0D+00 * epsmch
    epslog = log10 ( eps )

    err = 0.0D+00

    do j = 1, n
      temp = abs ( x(j) )
      if ( temp == 0.0D+00 ) then
        temp = 1.0D+00
      end if
      err(1:m) = err(1:m) + temp * fjac(1:m,j)
    end do

    do i = 1, m

      temp = 1.0D+00

      if ( fvec(i) /= 0.0D+00 .and. fvecp(i) /= 0.0D+00 .and. &
        abs ( fvecp(i)-fvec(i)) >= epsf * abs ( fvec(i) ) ) then
        temp = eps * abs ( (fvecp(i)-fvec(i)) / eps - err(i) ) &
          / ( abs ( fvec(i) ) + abs ( fvecp(i) ) )
      end if

      err(i) = 1.0D+00

      if ( epsmch < temp .and. temp < eps ) then
        err(i) = ( log10 ( temp ) - epslog ) / epslog
      end if

      if ( eps <= temp ) then
        err(i) = 0.0D+00
      end if

    end do

  end if

  return
end subroutine chkder
subroutine dogleg ( n, r, lr, diag, qtb, delta, x )





























































  implicit none

  integer ( kind = 4 ) lr
  integer ( kind = 4 ) n

  real ( kind = 8 ) alpha
  real ( kind = 8 ) bnorm
  real ( kind = 8 ) delta
  real ( kind = 8 ) diag(n)
  real ( kind = 8 ) enorm
  real ( kind = 8 ) epsmch
  real ( kind = 8 ) gnorm
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jj
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  real ( kind = 8 ) qnorm
  real ( kind = 8 ) qtb(n)
  real ( kind = 8 ) r(lr)
  real ( kind = 8 ) sgnorm
  real ( kind = 8 ) sum2
  real ( kind = 8 ) temp
  real ( kind = 8 ) wa1(n)
  real ( kind = 8 ) wa2(n)
  real ( kind = 8 ) x(n)

  epsmch = epsilon ( epsmch )



  jj = ( n * ( n + 1 ) ) / 2 + 1

  do k = 1, n

     j = n - k + 1
     jj = jj - k
     l = jj + 1
     sum2 = 0.0D+00

     do i = j + 1, n
       sum2 = sum2 + r(l) * x(i)
       l = l + 1
     end do

     temp = r(jj)

     if ( temp == 0.0D+00 ) then

       l = j
       do i = 1, j
         temp = max ( temp, abs ( r(l)) )
         l = l + n - i
       end do

       if ( temp == 0.0D+00 ) then
         temp = epsmch
       else
         temp = epsmch * temp
       end if

     end if

     x(j) = ( qtb(j) - sum2 ) / temp

  end do



  wa1(1:n) = 0.0D+00
  wa2(1:n) = diag(1:n) * x(1:n)
  qnorm = enorm ( n, wa2 )

  if ( qnorm <= delta ) then
    return
  end if




  l = 1
  do j = 1, n
     temp = qtb(j)
     do i = j, n
       wa1(i) = wa1(i) + r(l) * temp
       l = l + 1
     end do
     wa1(j) = wa1(j) / diag(j)
  end do




  gnorm = enorm ( n, wa1 )
  sgnorm = 0.0D+00
  alpha = delta / qnorm

  if ( gnorm /= 0.0D+00 ) then



    wa1(1:n) = ( wa1(1:n) / gnorm ) / diag(1:n)

    l = 1
    do j = 1, n
      sum2 = 0.0D+00
      do i = j, n
        sum2 = sum2 + r(l) * wa1(i)
        l = l + 1
      end do
      wa2(j) = sum2
    end do

    temp = enorm ( n, wa2 )
    sgnorm = ( gnorm / temp ) / temp



    alpha = 0.0D+00




    if ( sgnorm < delta ) then

      bnorm = enorm ( n, qtb )
      temp = ( bnorm / gnorm ) * ( bnorm / qnorm ) * ( sgnorm / delta )
      temp = temp - ( delta / qnorm ) * ( sgnorm / delta) ** 2 &
        + sqrt ( ( temp - ( delta / qnorm ) ) ** 2 &
        + ( 1.0D+00 - ( delta / qnorm ) ** 2 ) &
        * ( 1.0D+00 - ( sgnorm / delta ) ** 2 ) )

      alpha = ( ( delta / qnorm ) * ( 1.0D+00 - ( sgnorm / delta ) ** 2 ) ) &
        / temp

    end if

  end if




  temp = ( 1.0D+00 - alpha ) * min ( sgnorm, delta )

  x(1:n) = temp * wa1(1:n) + alpha * x(1:n)

  return
end subroutine dogleg
function enorm ( n, x )






































  implicit none

  integer ( kind = 4 ) n
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) enorm

  enorm = sqrt ( sum ( x(1:n) ** 2 ))

  return
end function enorm
function enorm2 ( n, x )


















































  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) agiant
  real ( kind = 8 ) enorm2
  integer ( kind = 4 ) i
  real ( kind = 8 ) rdwarf
  real ( kind = 8 ) rgiant
  real ( kind = 8 ) s1
  real ( kind = 8 ) s2
  real ( kind = 8 ) s3
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xabs
  real ( kind = 8 ) x1max
  real ( kind = 8 ) x3max

  rdwarf = sqrt ( tiny ( rdwarf ) )
  rgiant = sqrt ( huge ( rgiant ) )

  s1 = 0.0D+00
  s2 = 0.0D+00
  s3 = 0.0D+00
  x1max = 0.0D+00
  x3max = 0.0D+00
  agiant = rgiant / real ( n, kind = 8 )

  do i = 1, n

    xabs = abs ( x(i) )

    if ( xabs <= rdwarf ) then

      if ( x3max < xabs ) then
        s3 = 1.0D+00 + s3 * ( x3max / xabs ) ** 2
        x3max = xabs
      else if ( xabs /= 0.0D+00 ) then
        s3 = s3 + ( xabs / x3max ) ** 2
      end if

    else if ( agiant <= xabs ) then

      if ( x1max < xabs ) then
        s1 = 1.0D+00 + s1 * ( x1max / xabs ) ** 2
        x1max = xabs
      else
        s1 = s1 + ( xabs / x1max ) ** 2
      end if

    else

      s2 = s2 + xabs ** 2

    end if

  end do



  if ( s1 /= 0.0D+00 ) then

    enorm2 = x1max * sqrt ( s1 + ( s2 / x1max ) / x1max )

  else if ( s2 /= 0.0D+00 ) then

    if ( x3max <= s2 ) then
      enorm2 = sqrt ( s2 * ( 1.0D+00 + ( x3max / s2 ) * ( x3max * s3 ) ) )
    else
      enorm2 = sqrt ( x3max * ( ( s2 / x3max ) + ( x3max * s3 ) ) )
    end if

  else

    enorm2 = x3max * sqrt ( s3 )

  end if

  return
end function enorm2
subroutine fdjac1 ( fcn, n, x, fvec, fjac, ldfjac, iflag, ml, mu, epsfcn, m, prms )












































































  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) n
  integer ( kind = 4 ) m

  real ( kind = 8 ) eps
  real ( kind = 8 ) epsfcn
  real ( kind = 8 ) epsmch
  external fcn
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fvec(n)
  real ( kind = 8 ) h
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) ml
  integer ( kind = 4 ) msum
  integer ( kind = 4 ) mu
  real ( kind = 8 ) temp
  real ( kind = 8 ) wa1(n)
  real ( kind = 8 ) wa2(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) prms(m)

  epsmch = epsilon ( epsmch )

  eps = sqrt ( max ( epsfcn, epsmch ) )
  msum = ml + mu + 1



  if ( n <= msum ) then

     do j = 1, n

        temp = x(j)
        h = eps * abs ( temp )
        if ( h == 0.0D+00 ) then
          h = eps
        end if

        iflag = 1
        x(j) = temp + h
        call fcn ( n, x, wa1, iflag, m, prms )

        if ( iflag < 0 ) then
          exit
        end if

        x(j) = temp
        fjac(1:n,j) = ( wa1(1:n) - fvec(1:n) ) / h

     end do

  else



     do k = 1, msum

        do j = k, n, msum
          wa2(j) = x(j)
          h = eps * abs ( wa2(j) )
          if ( h == 0.0D+00 ) then
            h = eps
          end if
          x(j) = wa2(j) + h
        end do

        iflag = 1
        call fcn ( n, x, wa1, iflag, m, prms )

        if ( iflag < 0 ) then
          exit
        end if

        do j = k, n, msum

           x(j) = wa2(j)

           h = eps * abs ( wa2(j) )
           if ( h == 0.0D+00 ) then
             h = eps
           end if

           fjac(1:n,j) = 0.0D+00

           do i = 1, n
             if ( j - mu <= i .and. i <= j + ml ) then
               fjac(i,j) = ( wa1(i) - fvec(i) ) / h
             end if
           end do

        end do

     end do

  end if

  return
end subroutine fdjac1
subroutine fdjac2 ( fcn, m, n, x, xdat, ydat, fvec, fjac, ldfjac, iflag, epsfcn )









































































  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) eps
  real ( kind = 8 ) epsfcn
  real ( kind = 8 ) epsmch
  external fcn
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fvec(m),xdat(m),ydat(m)
  real ( kind = 8 ) h
  
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) j
  real ( kind = 8 ) temp
  real ( kind = 8 ) wa(m)
  real ( kind = 8 ) x(n)

  epsmch = epsilon ( epsmch )

  eps = sqrt ( max ( epsfcn, epsmch ) )

  do j = 1, n

    temp = x(j)
    h = eps * abs ( temp )
    if ( h == 0.0D+00 ) then
      h = eps
    end if

    iflag = 1
    x(j) = temp + h
    call fcn ( m, n, x, xdat, ydat, wa, iflag )

    if ( iflag < 0 ) then
      exit
    end if

    x(j) = temp
    fjac(1:m,j) = ( wa(1:m) - fvec(1:m) ) / h

  end do

  return
end subroutine fdjac2


subroutine hybrd ( fcn, n, x, fvec, xtol, maxfev, ml, mu, epsfcn, diag, mode, &
  factor, nprint, info, nfev, fjac, ldfjac, r, lr, qtf, m, prms )





























































































































  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) lr
  integer ( kind = 4 ) n
  integer ( kind = 4 ) m

  real ( kind = 8 ) actred
  real ( kind = 8 ) delta
  real ( kind = 8 ) diag(n)
  real ( kind = 8 ) enorm
  real ( kind = 8 ) epsfcn
  real ( kind = 8 ) epsmch
  real ( kind = 8 ) factor
  external fcn
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fnorm
  real ( kind = 8 ) fnorm1
  real ( kind = 8 ) fvec(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) info
  integer ( kind = 4 ) iter
  integer ( kind = 4 ) iwa(1)
  integer ( kind = 4 ) j
  logical jeval
  integer ( kind = 4 ) l
  integer ( kind = 4 ) maxfev
  integer ( kind = 4 ) ml
  integer ( kind = 4 ) mode
  integer ( kind = 4 ) msum
  integer ( kind = 4 ) mu
  integer ( kind = 4 ) ncfail
  integer ( kind = 4 ) nslow1
  integer ( kind = 4 ) nslow2
  integer ( kind = 4 ) ncsuc
  integer ( kind = 4 ) nfev
  integer ( kind = 4 ) nprint
  logical pivot
  real ( kind = 8 ) pnorm
  real ( kind = 8 ) prered
  real ( kind = 8 ) qtf(n)
  real ( kind = 8 ) r(lr)
  real ( kind = 8 ) ratio
  logical sing
  real ( kind = 8 ) sum2
  real ( kind = 8 ) temp
  real ( kind = 8 ) wa1(n)
  real ( kind = 8 ) wa2(n)
  real ( kind = 8 ) wa3(n)
  real ( kind = 8 ) wa4(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xnorm
  real ( kind = 8 ) xtol
  real ( kind = 8 ) prms(m)

  epsmch = epsilon ( epsmch )

  info = 0
  iflag = 0
  nfev = 0



  if ( n <= 0 ) then
    return
  else if ( xtol < 0.0D+00 ) then
    return
  else if ( maxfev <= 0 ) then
    return
  else if ( ml < 0 ) then
    return
  else if ( mu < 0 ) then
    return
  else if ( factor <= 0.0D+00 ) then
    return
  else if ( ldfjac < n ) then
    return
  else if ( lr < ( n * ( n + 1 ) ) / 2 ) then
    return
  end if

  if ( mode == 2 ) then

    do j = 1, n
      if ( diag(j) <= 0.0D+00 ) then
        go to 300
      end if
    end do

  end if




  iflag = 1
  call fcn ( n, x, fvec, iflag, m, prms )
  nfev = 1

  if ( iflag < 0 ) then
    go to 300
  end if

  fnorm = enorm ( n, fvec )



  msum = min ( ml + mu + 1, n )



  iter = 1
  ncsuc = 0
  ncfail = 0
  nslow1 = 0
  nslow2 = 0



30 continue

    jeval = .true.



    iflag = 2
    call fdjac1 ( fcn, n, x, fvec, fjac, ldfjac, iflag, ml, mu, epsfcn, m, prms )

    nfev = nfev + msum

    if ( iflag < 0 ) then
      go to 300
    end if



    pivot = .false.
    call qrfac ( n, n, fjac, ldfjac, pivot, iwa, 1, wa1, wa2 )




    if ( iter == 1 ) then

      if ( mode /= 2 ) then

        diag(1:n) = wa2(1:n)
        do j = 1, n
          if ( wa2(j) == 0.0D+00 ) then
            diag(j) = 1.0D+00
          end if
        end do

      end if




      wa3(1:n) = diag(1:n) * x(1:n)
      xnorm = enorm ( n, wa3 )
      delta = factor * xnorm
      if ( delta == 0.0D+00 ) then
        delta = factor
      end if

    end if



     qtf(1:n) = fvec(1:n)

     do j = 1, n

       if ( fjac(j,j) /= 0.0D+00 ) then
         temp = - dot_product ( qtf(j:n), fjac(j:n,j) ) / fjac(j,j)
         qtf(j:n) = qtf(j:n) + fjac(j:n,j) * temp
       end if

     end do



     sing = .false.

     do j = 1, n
        l = j
        do i = 1, j - 1
          r(l) = fjac(i,j)
          l = l + n - i
        end do
        r(l) = wa1(j)
        if ( wa1(j) == 0.0D+00 ) then
          sing = .true.
        end if
     end do



     call qform ( n, n, fjac, ldfjac )



     if ( mode /= 2 ) then
       do j = 1, n
         diag(j) = max ( diag(j), wa2(j) )
       end do
     end if



180    continue



        if ( 0 < nprint ) then
          iflag = 0
          if ( mod ( iter - 1, nprint ) == 0 ) then
            call fcn ( n, x, fvec, iflag, m, prms )
          end if
          if ( iflag < 0 ) then
            go to 300
          end if
        end if



        call dogleg ( n, r, lr, diag, qtf, delta, wa1 )




        wa1(1:n) = - wa1(1:n)
        wa2(1:n) = x(1:n) + wa1(1:n)
        wa3(1:n) = diag(1:n) * wa1(1:n)

        pnorm = enorm ( n, wa3 )



        if ( iter == 1 ) then
          delta = min ( delta, pnorm )
        end if



        iflag = 1
        call fcn ( n, wa2, wa4, iflag, m, prms )
        nfev = nfev + 1

        if ( iflag < 0 ) then
          go to 300
        end if

        fnorm1 = enorm ( n, wa4 )



        actred = -1.0D+00
        if ( fnorm1 < fnorm ) then
          actred = 1.0D+00 - ( fnorm1 / fnorm ) ** 2
        endif



        l = 1
        do i = 1, n
          sum2 = 0.0D+00
          do j = i, n
            sum2 = sum2 + r(l) * wa1(j)
            l = l + 1
          end do
          wa3(i) = qtf(i) + sum2
        end do

        temp = enorm ( n, wa3 )
        prered = 0.0D+00
        if ( temp < fnorm ) then
          prered = 1.0D+00 - ( temp / fnorm ) ** 2
        end if



        ratio = 0.0D+00
        if ( 0.0D+00 < prered ) then
          ratio = actred / prered
        end if



        if ( ratio < 0.1D+00 ) then

          ncsuc = 0
          ncfail = ncfail + 1
          delta = 0.5D+00 * delta

        else

          ncfail = 0
          ncsuc = ncsuc + 1

          if ( 0.5D+00 <= ratio .or. 1 < ncsuc ) then
            delta = max ( delta, pnorm / 0.5D+00 )
          end if

          if ( abs ( ratio - 1.0D+00 ) <= 0.1D+00 ) then
            delta = pnorm / 0.5D+00
          end if

        end if






        if ( 0.0001D+00 <= ratio ) then
          x(1:n) = wa2(1:n)
          wa2(1:n) = diag(1:n) * x(1:n)
          fvec(1:n) = wa4(1:n)
          xnorm = enorm ( n, wa2 )
          fnorm = fnorm1
          iter = iter + 1
        end if



        nslow1 = nslow1 + 1
        if ( 0.001D+00 <= actred ) then
          nslow1 = 0
        end if

        if ( jeval ) then
          nslow2 = nslow2 + 1
        end if

        if ( 0.1D+00 <= actred ) then
          nslow2 = 0
        end if



        if ( delta <= xtol * xnorm .or. fnorm == 0.0D+00 ) then
          info = 1
        end if

        if ( info /= 0 ) then
          go to 300
        end if



        if ( maxfev <= nfev ) then
          info = 2
        end if

        if ( 0.1D+00 * max ( 0.1D+00 * delta, pnorm ) <= epsmch * xnorm ) then
          info = 3
        end if

        if ( nslow2 == 5 ) then
          info = 4
        end if

        if ( nslow1 == 10 ) then
          info = 5
        end if

        if ( info /= 0 ) then
          go to 300
        end if




        if ( ncfail == 2 ) then
          go to 290
        end if




        do j = 1, n
          sum2 = dot_product ( wa4(1:n), fjac(1:n,j) )
          wa2(j) = ( sum2 - wa3(j) ) / pnorm
          wa1(j) = diag(j) * ( ( diag(j) * wa1(j) ) / pnorm )
          if ( 0.0001D+00 <= ratio ) then
            qtf(j) = sum2
          end if
        end do



        call r1updt ( n, n, r, lr, wa1, wa2, wa3, sing )
        call r1mpyq ( n, n, fjac, ldfjac, wa2, wa3 )
        call r1mpyq ( 1, n, qtf, 1, wa2, wa3 )



        jeval = .false.
        go to 180

  290   continue



     go to 30

  300 continue



  if ( iflag < 0 ) then
    info = iflag
  end if

  iflag = 0

  if ( 0 < nprint ) then
    call fcn ( n, x, fvec, iflag, m, prms )
  end if

  return
end subroutine hybrd


subroutine hybrd1 ( fcn, n, x, fvec, tol, info, m, prms )















































































  implicit none

  
  integer ( kind = 4 ) n
  integer ( kind = 4 ) m

  real ( kind = 8 ) diag(n)
  real ( kind = 8 ) epsfcn
  real ( kind = 8 ) factor
  external fcn
  real ( kind = 8 ) fjac(n,n)
  real ( kind = 8 ) fvec(n)
  integer ( kind = 4 ) info
  
  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) lr
  integer ( kind = 4 ) maxfev
  integer ( kind = 4 ) ml
  integer ( kind = 4 ) mode
  integer ( kind = 4 ) mu
  integer ( kind = 4 ) nfev
  integer ( kind = 4 ) nprint
  real ( kind = 8 ) qtf(n)
  real ( kind = 8 ) r((n*(n+1))/2)
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xtol
  real ( kind = 8 ) prms(m)

  if ( n <= 0 ) then
    info = 0
    return
  end if

  if ( tol < 0.0D+00 ) then
    info = 0
    return
  end if

  xtol = tol
  maxfev = 200 * ( n + 1 )
  ml = n - 1
  mu = n - 1
  epsfcn = 0.0D+00
  diag(1:n) = 1.0D+00
  mode = 2
  factor = 100.0D+00
  nprint = 0
  info = 0
  nfev = 0
  fjac(1:n,1:n) = 0.0D+00
  ldfjac = n
  r(1:(n*(n+1))/2) = 0.0D+00
  lr = ( n * ( n + 1 ) ) / 2
  qtf(1:n) = 0.0D+00

  call hybrd ( fcn, n, x, fvec, xtol, maxfev, ml, mu, epsfcn, diag, mode, &
    factor, nprint, info, nfev, fjac, ldfjac, r, lr, qtf, m, prms )

  if ( info == 5 ) then
    info = 4
  end if

  return
end subroutine hybrd1
subroutine hybrj ( fcn, n, x, fvec, fjac, ldfjac, xtol, maxfev, diag, mode, &
  factor, nprint, info, nfev, njev, r, lr, qtf )

























































































































  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) lr
  integer ( kind = 4 ) n

  real ( kind = 8 ) actred
  real ( kind = 8 ) delta
  real ( kind = 8 ) diag(n)
  real ( kind = 8 ) enorm
  real ( kind = 8 ) epsmch
  real ( kind = 8 ) factor
  external fcn
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fnorm
  real ( kind = 8 ) fnorm1
  real ( kind = 8 ) fvec(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) info
  integer ( kind = 4 ) iter
  integer ( kind = 4 ) iwa(1)
  integer ( kind = 4 ) j
  logical jeval
  integer ( kind = 4 ) l
  integer ( kind = 4 ) maxfev
  integer ( kind = 4 ) mode
  integer ( kind = 4 ) ncfail
  integer ( kind = 4 ) nslow1
  integer ( kind = 4 ) nslow2
  integer ( kind = 4 ) ncsuc
  integer ( kind = 4 ) nfev
  integer ( kind = 4 ) njev
  integer ( kind = 4 ) nprint
  logical pivot
  real ( kind = 8 ) pnorm
  real ( kind = 8 ) prered
  real ( kind = 8 ) qtf(n)
  real ( kind = 8 ) r(lr)
  real ( kind = 8 ) ratio
  logical sing
  real ( kind = 8 ) sum2
  real ( kind = 8 ) temp
  real ( kind = 8 ) wa1(n)
  real ( kind = 8 ) wa2(n)
  real ( kind = 8 ) wa3(n)
  real ( kind = 8 ) wa4(n)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xnorm
  real ( kind = 8 ) xtol

  epsmch = epsilon ( epsmch )

  info = 0
  iflag = 0
  nfev = 0
  njev = 0



  if ( n <= 0 ) then
    if ( iflag < 0 ) then
      info = iflag
    end if
    iflag = 0
    if ( 0 < nprint ) then
      call fcn ( n, x, fvec, fjac, ldfjac, iflag )
    end if
    return
  end if

  if ( ldfjac < n .or. &
       xtol < 0.0D+00 .or. &
       maxfev <= 0 .or. &
       factor <= 0.0D+00 .or. &
       lr < ( n * ( n + 1 ) ) / 2 ) then
    if ( iflag < 0 ) then
      info = iflag
    end if
    iflag = 0
    if ( 0 < nprint ) then
      call fcn ( n, x, fvec, fjac, ldfjac, iflag )
    end if
    return
  end if

  if ( mode == 2 ) then
    do j = 1, n
      if ( diag(j) <= 0.0D+00 ) then
        if ( iflag < 0 ) then
          info = iflag
        end if
        iflag = 0
        if ( 0 < nprint ) then
          call fcn ( n, x, fvec, fjac, ldfjac, iflag )
        end if
        return
      end if
    end do
  end if



  iflag = 1
  call fcn ( n, x, fvec, fjac, ldfjac, iflag )
  nfev = 1

  if ( iflag < 0 ) then
    if ( iflag < 0 ) then
      info = iflag
    end if
    iflag = 0
    if ( 0 < nprint ) then
      call fcn ( n, x, fvec, fjac, ldfjac, iflag )
    end if
    return
  end if

  fnorm = enorm ( n, fvec )



  iter = 1
  ncsuc = 0
  ncfail = 0
  nslow1 = 0
  nslow2 = 0



  do

    jeval = .true.



    iflag = 2
    call fcn ( n, x, fvec, fjac, ldfjac, iflag )
    njev = njev + 1

    if ( iflag < 0 ) then
      info = iflag
      iflag = 0
      if ( 0 < nprint ) then
        call fcn ( n, x, fvec, fjac, ldfjac, iflag )
      end if
      return
    end if



    pivot = .false.
    call qrfac ( n, n, fjac, ldfjac, pivot, iwa, 1, wa1, wa2 )




    if ( iter == 1 ) then

      if ( mode /= 2 ) then
        diag(1:n) = wa2(1:n)
        do j = 1, n
          if ( wa2(j) == 0.0D+00 ) then
            diag(j) = 1.0D+00
          end if
        end do
      end if




      wa3(1:n) = diag(1:n) * x(1:n)
      xnorm = enorm ( n, wa3 )
      delta = factor * xnorm
      if ( delta == 0.0D+00 ) then
        delta = factor
      end if

    end if



    qtf(1:n) = fvec(1:n)

    do j = 1, n
      if ( fjac(j,j) /= 0.0D+00 ) then
        sum2 = 0.0D+00
        do i = j, n
          sum2 = sum2 + fjac(i,j) * qtf(i)
        end do
        temp = - sum2 / fjac(j,j)
        do i = j, n
          qtf(i) = qtf(i) + fjac(i,j) * temp
        end do
      end if
    end do



    sing = .false.

    do j = 1, n
      l = j
      do i = 1, j - 1
        r(l) = fjac(i,j)
        l = l + n - i
      end do
      r(l) = wa1(j)
      if ( wa1(j) == 0.0D+00 ) then
        sing = .true.
      end if
    end do



    call qform ( n, n, fjac, ldfjac )



    if ( mode /= 2 ) then
      do j = 1, n
        diag(j) = max ( diag(j), wa2(j) )
      end do
    end if



    do



      if ( 0 < nprint ) then

        iflag = 0
        if ( mod ( iter - 1, nprint ) == 0 ) then
          call fcn ( n, x, fvec, fjac, ldfjac, iflag )
        end if

        if ( iflag < 0 ) then
          info = iflag
          iflag = 0
          if ( 0 < nprint ) then
            call fcn ( n, x, fvec, fjac, ldfjac, iflag )
          end if
          return
        end if

      end if



      call dogleg ( n, r, lr, diag, qtf, delta, wa1 )




      wa1(1:n) = - wa1(1:n)
      wa2(1:n) = x(1:n) + wa1(1:n)
      wa3(1:n) = diag(1:n) * wa1(1:n)
      pnorm = enorm ( n, wa3 )



      if ( iter == 1 ) then
        delta = min ( delta, pnorm )
      end if



      iflag = 1
      call fcn ( n, wa2, wa4, fjac, ldfjac, iflag )
      nfev = nfev + 1

      if ( iflag < 0 ) then
        info = iflag
        iflag = 0
        if ( 0 < nprint ) then
          call fcn ( n, x, fvec, fjac, ldfjac, iflag )
        end if
        return
      end if

      fnorm1 = enorm ( n, wa4 )



      actred = -1.0D+00
      if ( fnorm1 < fnorm ) then
        actred = 1.0D+00 - ( fnorm1 / fnorm ) ** 2
      end if



      l = 1
      do i = 1, n
        sum2 = 0.0D+00
        do j = i, n
          sum2 = sum2 + r(l) * wa1(j)
          l = l + 1
        end do
        wa3(i) = qtf(i) + sum2
      end do

      temp = enorm ( n, wa3 )
      prered = 0.0D+00
      if ( temp < fnorm ) then
        prered = 1.0D+00 - ( temp / fnorm ) ** 2
      end if



      if ( 0.0D+00 < prered ) then
        ratio = actred / prered
      else
        ratio = 0.0D+00
      end if



      if ( ratio < 0.1D+00 ) then

        ncsuc = 0
        ncfail = ncfail + 1
        delta = 0.5D+00 * delta

      else

        ncfail = 0
        ncsuc = ncsuc + 1

        if ( 0.5D+00 <= ratio .or. 1 < ncsuc ) then
          delta = max ( delta, pnorm / 0.5D+00 )
        end if

        if ( abs ( ratio - 1.0D+00 ) <= 0.1D+00 ) then
          delta = pnorm / 0.5D+00
        end if

      end if








      if ( 0.0001D+00 <= ratio ) then
        x(1:n) = wa2(1:n)
        wa2(1:n) = diag(1:n) * x(1:n)
        fvec(1:n) = wa4(1:n)
        xnorm = enorm ( n, wa2 )
        fnorm = fnorm1
        iter = iter + 1
      end if



      nslow1 = nslow1 + 1
      if ( 0.001D+00 <= actred ) then
        nslow1 = 0
      end if

      if ( jeval ) then
        nslow2 = nslow2 + 1
      end if

      if ( 0.1D+00 <= actred ) then
        nslow2 = 0
      end if



      if ( delta <= xtol * xnorm .or. fnorm == 0.0D+00 ) then
        info = 1
      end if

      if ( info /= 0 ) then
        iflag = 0
        if ( 0 < nprint ) then
          call fcn ( n, x, fvec, fjac, ldfjac, iflag )
        end if
        return
      end if



      if ( maxfev <= nfev ) then
        info = 2
      end if

      if ( 0.1D+00 * max ( 0.1D+00 * delta, pnorm ) <= epsmch * xnorm ) then
        info = 3
      end if

      if ( nslow2 == 5 ) then
        info = 4
      end if

      if ( nslow1 == 10 ) then
        info = 5
      end if

      if ( info /= 0 ) then
        iflag = 0
        if ( 0 < nprint ) then
          call fcn ( n, x, fvec, fjac, ldfjac, iflag )
        end if
        return
      end if



      if ( ncfail == 2 ) then
        exit
      end if




      do j = 1, n
        sum2 = dot_product ( wa4(1:n), fjac(1:n,j) )
        wa2(j) = ( sum2 - wa3(j) ) / pnorm
        wa1(j) = diag(j) * ( ( diag(j) * wa1(j) ) / pnorm )
        if ( 0.0001D+00 <= ratio ) then
          qtf(j) = sum2
        end if
      end do



      call r1updt ( n, n, r, lr, wa1, wa2, wa3, sing )
      call r1mpyq ( n, n, fjac, ldfjac, wa2, wa3 )
      call r1mpyq ( 1, n, qtf, 1, wa2, wa3 )



      jeval = .false.

    end do



  end do

end subroutine hybrj
subroutine hybrj1 ( fcn, n, x, fvec, fjac, ldfjac, tol, info )



















































































  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) n

  real ( kind = 8 ) diag(n)
  real ( kind = 8 ) factor
  external fcn
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fvec(n)
  integer ( kind = 4 ) info
  
  integer ( kind = 4 ) lr
  integer ( kind = 4 ) maxfev
  integer ( kind = 4 ) mode
  integer ( kind = 4 ) nfev
  integer ( kind = 4 ) njev
  integer ( kind = 4 ) nprint
  real ( kind = 8 ) qtf(n)
  real ( kind = 8 ) r((n*(n+1))/2)
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xtol

  info = 0

  if ( n <= 0 ) then
    return
  else if ( ldfjac < n ) then
    return
  else if ( tol < 0.0D+00 ) then
    return
  end if

  maxfev = 100 * ( n + 1 )
  xtol = tol
  mode = 2
  diag(1:n) = 1.0D+00
  factor = 100.0D+00
  nprint = 0
  lr = ( n * ( n + 1 ) ) / 2

  call hybrj ( fcn, n, x, fvec, fjac, ldfjac, xtol, maxfev, diag, mode, &
    factor, nprint, info, nfev, njev, r, lr, qtf )

  if ( info == 5 ) then
    info = 4
  end if

  return
end subroutine hybrj1
subroutine lmder ( fcn, m, n, x, fvec, fjac, ldfjac, ftol, xtol, gtol, maxfev, &
  diag, mode, factor, nprint, info, nfev, njev, ipvt, qtf )

















































































































































  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) actred
  real ( kind = 8 ) delta
  real ( kind = 8 ) diag(n)
  real ( kind = 8 ) dirder
  real ( kind = 8 ) enorm
  real ( kind = 8 ) epsmch
  real ( kind = 8 ) factor
  external fcn
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fnorm
  real ( kind = 8 ) fnorm1
  real ( kind = 8 ) ftol
  real ( kind = 8 ) fvec(m)
  real ( kind = 8 ) gnorm
  real ( kind = 8 ) gtol
  
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) iter
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  integer ( kind = 4 ) maxfev
  integer ( kind = 4 ) mode
  integer ( kind = 4 ) nfev
  integer ( kind = 4 ) njev
  integer ( kind = 4 ) nprint
  real ( kind = 8 ) par
  logical pivot
  real ( kind = 8 ) pnorm
  real ( kind = 8 ) prered
  real ( kind = 8 ) qtf(n)
  real ( kind = 8 ) ratio
  real ( kind = 8 ) sum2
  real ( kind = 8 ) temp
  real ( kind = 8 ) temp1
  real ( kind = 8 ) temp2
  real ( kind = 8 ) wa1(n)
  real ( kind = 8 ) wa2(n)
  real ( kind = 8 ) wa3(n)
  real ( kind = 8 ) wa4(m)
  real ( kind = 8 ) xnorm
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xtol

  epsmch = epsilon ( epsmch )

  info = 0
  iflag = 0
  nfev = 0
  njev = 0



  if ( n <= 0 ) then
    go to 300
  end if

  if ( m < n ) then
    go to 300
  end if

  if ( ldfjac < m &
    .or. ftol < 0.0D+00 .or. xtol < 0.0D+00 .or. gtol < 0.0D+00 &
     .or. maxfev <= 0 .or. factor <= 0.0D+00 ) then
    go to 300
  end if

  if ( mode == 2 ) then
    do j = 1, n
      if ( diag(j) <= 0.0D+00 ) then
        go to 300
      end if
    end do
  end if



  iflag = 1
  call fcn ( m, n, x, fvec, fjac, ldfjac, iflag )
  nfev = 1
  if ( iflag < 0 ) then
    go to 300
  end if

  fnorm = enorm ( m, fvec )



  par = 0.0D+00
  iter = 1



30   continue



    iflag = 2
    call fcn ( m, n, x, fvec, fjac, ldfjac, iflag )

    njev = njev + 1

    if ( iflag < 0 ) then
      go to 300
    end if



    if ( 0 < nprint ) then
      iflag = 0
      if ( mod ( iter - 1, nprint ) == 0 ) then
        call fcn ( m, n, x, fvec, fjac, ldfjac, iflag )
      end if
      if ( iflag < 0 ) then
        go to 300
      end if
    end if



    pivot = .true.
    call qrfac ( m, n, fjac, ldfjac, pivot, ipvt, n, wa1, wa2 )




    if ( iter == 1 ) then

      if ( mode /= 2 ) then
        diag(1:n) = wa2(1:n)
        do j = 1, n
          if ( wa2(j) == 0.0D+00 ) then
            diag(j) = 1.0D+00
          end if
        end do
      end if




      wa3(1:n) = diag(1:n) * x(1:n)

      xnorm = enorm ( n, wa3 )
      delta = factor * xnorm
      if ( delta == 0.0D+00 ) then
        delta = factor
      end if

    end if



    wa4(1:m) = fvec(1:m)

    do j = 1, n

      if ( fjac(j,j) /= 0.0D+00 ) then
        sum2 = dot_product ( wa4(j:m), fjac(j:m,j) )
        temp = - sum2 / fjac(j,j)
        wa4(j:m) = wa4(j:m) + fjac(j:m,j) * temp
      end if

      fjac(j,j) = wa1(j)
      qtf(j) = wa4(j)

    end do



    gnorm = 0.0D+00

    if ( fnorm /= 0.0D+00 ) then

      do j = 1, n
        l = ipvt(j)
        if ( wa2(l) /= 0.0D+00 ) then
          sum2 = dot_product ( qtf(1:j), fjac(1:j,j) ) / fnorm
          gnorm = max ( gnorm, abs ( sum2 / wa2(l) ) )
        end if
      end do

    end if



    if ( gnorm <= gtol ) then
      info = 4
      go to 300
    end if



    if ( mode /= 2 ) then
      do j = 1, n
        diag(j) = max ( diag(j), wa2(j) )
      end do
    end if



200    continue



    call lmpar ( n, fjac, ldfjac, ipvt, diag, qtf, delta, par, wa1, wa2 )



    wa1(1:n) = - wa1(1:n)
    wa2(1:n) = x(1:n) + wa1(1:n)
    wa3(1:n) = diag(1:n) * wa1(1:n)

    pnorm = enorm ( n, wa3 )



    if ( iter == 1 ) then
      delta = min ( delta, pnorm )
    end if



    iflag = 1
    call fcn ( m, n, wa2, wa4, fjac, ldfjac, iflag )

    nfev = nfev + 1

    if ( iflag < 0 ) then
      go to 300
    end if

    fnorm1 = enorm ( m, wa4 )



    actred = -1.0D+00
    if ( 0.1D+00 * fnorm1 < fnorm ) then
      actred = 1.0D+00 - ( fnorm1 / fnorm ) ** 2
    end if




    do j = 1, n
      wa3(j) = 0.0D+00
      l = ipvt(j)
      temp = wa1(l)
      wa3(1:j) = wa3(1:j) + fjac(1:j,j) * temp
    end do

    temp1 = enorm ( n, wa3 ) / fnorm
    temp2 = ( sqrt ( par ) * pnorm ) / fnorm
    prered = temp1 ** 2 + temp2 ** 2 / 0.5D+00
    dirder = - ( temp1 ** 2 + temp2 ** 2 )



    if ( prered /= 0.0D+00 ) then
      ratio = actred / prered
    else
      ratio = 0.0D+00
    end if



    if ( ratio <= 0.25D+00 ) then

      if ( 0.0D+00 <= actred ) then
        temp = 0.5D+00
      end if

      if ( actred < 0.0D+00 ) then
        temp = 0.5D+00 * dirder / ( dirder + 0.5D+00 * actred )
      end if

      if ( 0.1D+00 * fnorm1 >= fnorm .or. temp < 0.1D+00 ) then
        temp = 0.1D+00
      end if

      delta = temp * min ( delta, pnorm / 0.1D+00 )
      par = par / temp

    else

      if ( par == 0.0D+00 .or. ratio >= 0.75D+00 ) then
        delta = 2.0D+00 * pnorm
        par = 0.5D+00 * par
      end if

    end if





    if ( 0.0001D+00 <= ratio ) then
      x(1:n) = wa2(1:n)
      wa2(1:n) = diag(1:n) * x(1:n)
      fvec(1:m) = wa4(1:m)
      xnorm = enorm ( n, wa2 )
      fnorm = fnorm1
      iter = iter + 1
    end if



    if ( abs ( actred) <= ftol .and. &
      prered <= ftol .and. &
      0.5D+00 * ratio <= 1.0D+00 ) then
      info = 1
    end if

    if ( delta <= xtol * xnorm ) then
      info = 2
    end if

    if ( abs ( actred) <= ftol .and. prered <= ftol &
      .and. 0.5D+00 * ratio <= 1.0D+00 .and. info == 2 ) then
      info = 3
    end if

    if ( info /= 0 ) then
      go to 300
    end if



    if ( nfev >= maxfev ) then
      info = 5
    end if

    if ( abs ( actred ) <= epsmch .and. prered <= epsmch &
      .and. 0.5D+00 * ratio <= 1.0D+00 ) then
      info = 6
    end if

    if ( delta <= epsmch * xnorm ) then
      info = 7
    end if

    if ( gnorm <= epsmch ) then
      info = 8
    end if

    if ( info /= 0 ) then
      go to 300
    end if



    if ( ratio < 0.0001D+00 ) then
      go to 200
    end if



    go to 30

  300 continue



  if ( iflag < 0 ) then
    info = iflag
  end if

  iflag = 0

  if ( 0 < nprint ) then
    call fcn ( m, n, x, fvec, fjac, ldfjac, iflag )
  end if

  return
end subroutine lmder
subroutine lmder1 ( fcn, m, n, x, fvec, fjac, ldfjac, tol, info )

































































































  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) diag(n)
  real ( kind = 8 ) factor
  external fcn
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) ftol
  real ( kind = 8 ) fvec(m)
  real ( kind = 8 ) gtol
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) maxfev
  integer ( kind = 4 ) mode
  integer ( kind = 4 ) nfev
  integer ( kind = 4 ) njev
  integer ( kind = 4 ) nprint
  real ( kind = 8 ) qtf(n)
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xtol

  info = 0

  if ( n <= 0 ) then
    return
  else if ( m < n ) then
    return
  else if ( ldfjac < m ) then
    return
  else if ( tol < 0.0D+00 ) then
    return
  end if

  factor = 100.0D+00
  maxfev = 100 * ( n + 1 )
  ftol = tol
  xtol = tol
  gtol = 0.0D+00
  mode = 1
  nprint = 0

  call lmder ( fcn, m, n, x, fvec, fjac, ldfjac, ftol, xtol, gtol, maxfev, &
    diag, mode, factor, nprint, info, nfev, njev, ipvt, qtf )

  if ( info == 8 ) then
    info = 4
  end if

  return
end subroutine lmder1
subroutine lmdif ( fcn, m, n, x, xdat, ydat, fvec, ftol, xtol, gtol, maxfev, epsfcn, &
  diag, mode, factor, nprint, info, nfev, fjac, ldfjac, ipvt, qtf )























































































































































  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) actred
  real ( kind = 8 ) delta
  real ( kind = 8 ) diag(n)
  real ( kind = 8 ) dirder
  real ( kind = 8 ) enorm
  real ( kind = 8 ) epsfcn
  real ( kind = 8 ) epsmch
  real ( kind = 8 ) factor
  external fcn
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fnorm
  real ( kind = 8 ) fnorm1
  real ( kind = 8 ) ftol
  real ( kind = 8 ) :: fvec(m),xdat(m),ydat(m)
  real ( kind = 8 ) gnorm
  real ( kind = 8 ) gtol
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) iter
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  integer ( kind = 4 ) maxfev
  integer ( kind = 4 ) mode
  integer ( kind = 4 ) nfev
  integer ( kind = 4 ) nprint
  real ( kind = 8 ) par
  logical pivot
  real ( kind = 8 ) pnorm
  real ( kind = 8 ) prered
  real ( kind = 8 ) qtf(n)
  real ( kind = 8 ) ratio
  real ( kind = 8 ) sum2
  real ( kind = 8 ) temp
  real ( kind = 8 ) temp1
  real ( kind = 8 ) temp2
  real ( kind = 8 ) wa1(n)
  real ( kind = 8 ) wa2(n)
  real ( kind = 8 ) wa3(n)
  real ( kind = 8 ) wa4(m)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xnorm
  real ( kind = 8 ) xtol

  epsmch = epsilon ( epsmch )

  info = 0
  iflag = 0
  nfev = 0

  if ( n <= 0 ) then
    go to 300
  else if ( m < n ) then
    go to 300
  else if ( ldfjac < m ) then
    go to 300
  else if ( ftol < 0.0D+00 ) then
    go to 300
  else if ( xtol < 0.0D+00 ) then
    go to 300
  else if ( gtol < 0.0D+00 ) then
    go to 300
  else if ( maxfev <= 0 ) then
    go to 300
  else if ( factor <= 0.0D+00 ) then
    go to 300
  end if

  if ( mode == 2 ) then
    do j = 1, n
      if ( diag(j) <= 0.0D+00 ) then
        go to 300
      end if
    end do
  end if



  iflag = 1
  call fcn ( m, n, x, xdat, ydat, fvec, iflag )
  nfev = 1

  if ( iflag < 0 ) then
    go to 300
  end if

  fnorm = enorm ( m, fvec )



  par = 0.0D+00
  iter = 1



30 continue



  iflag = 2
  call fdjac2 ( fcn, m, n, x, xdat, ydat, fvec, fjac, ldfjac, iflag, epsfcn )
  nfev = nfev + n

  if ( iflag < 0 ) then
    go to 300
  end if



  if ( 0 < nprint ) then
    iflag = 0
    if ( mod ( iter - 1, nprint ) == 0 ) then
      call fcn ( m, n, x, xdat, ydat, fvec, iflag )
    end if
    if ( iflag < 0 ) then
      go to 300
    end if
  end if



  pivot = .true.
  call qrfac ( m, n, fjac, ldfjac, pivot, ipvt, n, wa1, wa2 )




     if ( iter == 1 ) then

       if ( mode /= 2 ) then
         diag(1:n) = wa2(1:n)
         do j = 1, n
           if ( wa2(j) == 0.0D+00 ) then
             diag(j) = 1.0D+00
           end if
         end do
       end if




       wa3(1:n) = diag(1:n) * x(1:n)
       xnorm = enorm ( n, wa3 )
       delta = factor * xnorm
       if ( delta == 0.0D+00 ) then
         delta = factor
       end if
     end if



     wa4(1:m) = fvec(1:m)

     do j = 1, n

       if ( fjac(j,j) /= 0.0D+00 ) then
         sum2 = dot_product ( wa4(j:m), fjac(j:m,j) )
         temp = - sum2 / fjac(j,j)
         wa4(j:m) = wa4(j:m) + fjac(j:m,j) * temp
       end if

       fjac(j,j) = wa1(j)
       qtf(j) = wa4(j)

     end do



     gnorm = 0.0D+00

     if ( fnorm /= 0.0D+00 ) then

       do j = 1, n

         l = ipvt(j)

         if ( wa2(l) /= 0.0D+00 ) then
           sum2 = 0.0D+00
           do i = 1, j
             sum2 = sum2 + fjac(i,j) * ( qtf(i) / fnorm )
           end do
           gnorm = max ( gnorm, abs ( sum2 / wa2(l) ) )
         end if

       end do

     end if



     if ( gnorm <= gtol ) then
       info = 4
       go to 300
     end if



     if ( mode /= 2 ) then
       do j = 1, n
         diag(j) = max ( diag(j), wa2(j) )
       end do
     end if



200  continue



        call lmpar ( n, fjac, ldfjac, ipvt, diag, qtf, delta, par, wa1, wa2 )




        wa1(1:n) = -wa1(1:n)
        wa2(1:n) = x(1:n) + wa1(1:n)
        wa3(1:n) = diag(1:n) * wa1(1:n)

        pnorm = enorm ( n, wa3 )



        if ( iter == 1 ) then
          delta = min ( delta, pnorm )
        end if



        iflag = 1
        call fcn ( m, n, wa2, xdat, ydat, wa4, iflag )
        nfev = nfev + 1
        if ( iflag < 0 ) then
          go to 300
        end if
        fnorm1 = enorm ( m, wa4 )



        if ( 0.1D+00 * fnorm1 < fnorm ) then
          actred = 1.0D+00 - ( fnorm1 / fnorm ) ** 2
        else
          actred = -1.0D+00
        end if



        do j = 1, n
          wa3(j) = 0.0D+00
          l = ipvt(j)
          temp = wa1(l)
          wa3(1:j) = wa3(1:j) + fjac(1:j,j) * temp
        end do

        temp1 = enorm ( n, wa3 ) / fnorm
        temp2 = ( sqrt ( par ) * pnorm ) / fnorm
        prered = temp1 ** 2 + temp2 ** 2 / 0.5D+00
        dirder = - ( temp1 ** 2 + temp2 ** 2 )



        ratio = 0.0D+00
        if ( prered /= 0.0D+00 ) then
          ratio = actred / prered
        end if



        if ( ratio <= 0.25D+00 ) then

           if ( actred >= 0.0D+00 ) then
             temp = 0.5D+00
           endif

           if ( actred < 0.0D+00 ) then
             temp = 0.5D+00 * dirder / ( dirder + 0.5D+00 * actred )
           end if

           if ( 0.1D+00 * fnorm1 >= fnorm .or. temp < 0.1D+00 ) then
             temp = 0.1D+00
           end if

           delta = temp * min ( delta, pnorm / 0.1D+00  )
           par = par / temp

        else

           if ( par == 0.0D+00 .or. ratio >= 0.75D+00 ) then
             delta = 2.0D+00 * pnorm
             par = 0.5D+00 * par
           end if

        end if







        if ( 0.0001D+00 <= ratio ) then
          x(1:n) = wa2(1:n)
          wa2(1:n) = diag(1:n) * x(1:n)
          fvec(1:m) = wa4(1:m)
          xnorm = enorm ( n, wa2 )
          fnorm = fnorm1
          iter = iter + 1
        end if



        if ( abs ( actred) <= ftol .and. prered <= ftol &
          .and. 0.5D+00 * ratio <= 1.0D+00 ) then
          info = 1
        end if

        if ( delta <= xtol * xnorm ) then
          info = 2
        end if

        if ( abs ( actred) <= ftol .and. prered <= ftol &
          .and. 0.5D+00 * ratio <= 1.0D+00 .and. info == 2 ) info = 3

        if ( info /= 0 ) then
          go to 300
        end if



        if ( maxfev <= nfev ) then
          info = 5
        end if

        if ( abs ( actred) <= epsmch .and. prered <= epsmch &
          .and. 0.5D+00 * ratio <= 1.0D+00 ) then
          info = 6
        end if

        if ( delta <= epsmch * xnorm ) then
          info = 7
        end if

        if ( gnorm <= epsmch ) then
          info = 8
        end if

        if ( info /= 0 ) then
          go to 300
        end if



        if ( ratio < 0.0001D+00 ) then
          go to 200
        end if



     go to 30

300 continue



  if ( iflag < 0 ) then
    info = iflag
  end if

  iflag = 0

  if ( 0 < nprint ) then
    call fcn ( m, n, x, xdat, ydat, fvec, iflag )
  end if

  return
end subroutine lmdif
subroutine lmdif1 ( fcn, m, n, x, xdat, ydat, fvec, tol, info )





















































































  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) diag(n)
  real ( kind = 8 ) epsfcn
  real ( kind = 8 ) factor
  external fcn
  real ( kind = 8 ) fjac(m,n)
  real ( kind = 8 ) ftol
  real ( kind = 8 ) ::fvec(m),xdat(m),ydat(m)
  real ( kind = 8 ) gtol
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) maxfev
  integer ( kind = 4 ) mode
  integer ( kind = 4 ) nfev
  integer ( kind = 4 ) nprint
  real ( kind = 8 ) qtf(n)
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xtol

  info = 0

  if ( n <= 0 ) then
    return
  else if ( m < n ) then
    return
  else if ( tol < 0.0D+00 ) then
    return
  end if

  factor = 100.0D+00
  maxfev = 200 * ( n + 1 )
  ftol = tol
  xtol = tol
  gtol = 0.0D+00
  epsfcn = 0.0D+00
  mode = 1
  nprint = 0
  ldfjac = m

  call lmdif ( fcn, m, n, x, xdat, ydat, fvec, ftol, xtol, gtol, maxfev, epsfcn, &
    diag, mode, factor, nprint, info, nfev, fjac, ldfjac, ipvt, qtf )

  if ( info == 8 ) then
    info = 4
  end if

  return
end subroutine lmdif1
subroutine lmpar ( n, r, ldr, ipvt, diag, qtb, delta, par, x, sdiag )

































































































  implicit none

  integer ( kind = 4 ) ldr
  integer ( kind = 4 ) n

  real ( kind = 8 ) delta
  real ( kind = 8 ) diag(n)
  real ( kind = 8 ) dwarf
  real ( kind = 8 ) dxnorm
  real ( kind = 8 ) enorm
  real ( kind = 8 ) gnorm
  real ( kind = 8 ) fp
  
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) iter
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) nsing
  real ( kind = 8 ) par
  real ( kind = 8 ) parc
  real ( kind = 8 ) parl
  real ( kind = 8 ) paru
  
  real ( kind = 8 ) qtb(n)
  real ( kind = 8 ) r(ldr,n)
  real ( kind = 8 ) sdiag(n)
  real ( kind = 8 ) sum2
  real ( kind = 8 ) temp
  real ( kind = 8 ) wa1(n)
  real ( kind = 8 ) wa2(n)
  real ( kind = 8 ) x(n)



  dwarf = tiny ( dwarf )





  nsing = n

  do j = 1, n
    wa1(j) = qtb(j)
    if ( r(j,j) == 0.0D+00 .and. nsing == n ) then
      nsing = j - 1
    end if
    if ( nsing < n ) then
      wa1(j) = 0.0D+00
    end if
  end do

  do k = 1, nsing
    j = nsing - k + 1
    wa1(j) = wa1(j) / r(j,j)
    temp = wa1(j)
    wa1(1:j-1) = wa1(1:j-1) - r(1:j-1,j) * temp
  end do

  do j = 1, n
    l = ipvt(j)
    x(l) = wa1(j)
  end do





  iter = 0
  wa2(1:n) = diag(1:n) * x(1:n)
  dxnorm = enorm ( n, wa2 )
  fp = dxnorm - delta

  if ( fp <= 0.1D+00 * delta ) then
    if ( iter == 0 ) then
      par = 0.0D+00
    end if
    return
  end if







  parl = 0.0D+00

  if ( n <= nsing ) then

    do j = 1, n
      l = ipvt(j)
      wa1(j) = diag(l) * ( wa2(l) / dxnorm )
    end do

    do j = 1, n
      sum2 = dot_product ( wa1(1:j-1), r(1:j-1,j) )
      wa1(j) = ( wa1(j) - sum2 ) / r(j,j)
    end do

    temp = enorm ( n, wa1 )
    parl = ( ( fp / delta ) / temp ) / temp

  end if



  do j = 1, n
    sum2 = dot_product ( qtb(1:j), r(1:j,j) )
    l = ipvt(j)
    wa1(j) = sum2 / diag(l)
  end do

  gnorm = enorm ( n, wa1 )
  paru = gnorm / delta

  if ( paru == 0.0D+00 ) then
    paru = dwarf / min ( delta, 0.1D+00 )
  end if




  par = max ( par, parl )
  par = min ( par, paru )
  if ( par == 0.0D+00 ) then
    par = gnorm / dxnorm
  end if



  do

    iter = iter + 1



    if ( par == 0.0D+00 ) then
      par = max ( dwarf, 0.001D+00 * paru )
    end if

    wa1(1:n) = sqrt ( par ) * diag(1:n)

    call qrsolv ( n, r, ldr, ipvt, wa1, qtb, x, sdiag )

    wa2(1:n) = diag(1:n) * x(1:n)
    dxnorm = enorm ( n, wa2 )
    temp = fp
    fp = dxnorm - delta



    if ( abs ( fp ) <= 0.1D+00 * delta ) then
      exit
    end if




    if ( parl == 0.0D+00 .and. fp <= temp .and. temp < 0.0D+00 ) then
      exit
    else if ( iter == 10 ) then
      exit
    end if



    do j = 1, n
      l = ipvt(j)
      wa1(j) = diag(l) * ( wa2(l) / dxnorm )
    end do

    do j = 1, n
      wa1(j) = wa1(j) / sdiag(j)
      temp = wa1(j)
      wa1(j+1:n) = wa1(j+1:n) - r(j+1:n,j) * temp
    end do

    temp = enorm ( n, wa1 )
    parc = ( ( fp / delta ) / temp ) / temp



    if ( 0.0D+00 < fp ) then
      parl = max ( parl, par )
    else if ( fp < 0.0D+00 ) then
      paru = min ( paru, par )
    end if



    par = max ( parl, par + parc )



  end do



  if ( iter == 0 ) then
    par = 0.0D+00
  end if

  return
end subroutine lmpar
subroutine lmstr ( fcn, m, n, x, fvec, fjac, ldfjac, ftol, xtol, gtol, maxfev, &
  diag, mode, factor, nprint, info, nfev, njev, ipvt, qtf )




















































































































































  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) actred
  real ( kind = 8 ) delta
  real ( kind = 8 ) diag(n)
  real ( kind = 8 ) dirder
  real ( kind = 8 ) enorm
  real ( kind = 8 ) epsmch
  real ( kind = 8 ) factor
  external fcn
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) fnorm
  real ( kind = 8 ) fnorm1
  real ( kind = 8 ) ftol
  real ( kind = 8 ) fvec(m)
  real ( kind = 8 ) gnorm
  real ( kind = 8 ) gtol
  integer ( kind = 4 ) i
  integer ( kind = 4 ) iflag
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) iter
  integer ( kind = 4 ) j
  integer ( kind = 4 ) l
  integer ( kind = 4 ) maxfev
  integer ( kind = 4 ) mode
  integer ( kind = 4 ) nfev
  integer ( kind = 4 ) njev
  integer ( kind = 4 ) nprint
  real ( kind = 8 ) par
  logical pivot
  real ( kind = 8 ) pnorm
  real ( kind = 8 ) prered
  real ( kind = 8 ) qtf(n)
  real ( kind = 8 ) ratio
  logical sing
  real ( kind = 8 ) sum2
  real ( kind = 8 ) temp
  real ( kind = 8 ) temp1
  real ( kind = 8 ) temp2
  real ( kind = 8 ) wa1(n)
  real ( kind = 8 ) wa2(n)
  real ( kind = 8 ) wa3(n)
  real ( kind = 8 ) wa4(m)
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xnorm
  real ( kind = 8 ) xtol

  epsmch = epsilon ( epsmch )

  info = 0
  iflag = 0
  nfev = 0
  njev = 0



  if ( n <= 0 ) then
    go to 340
  else if ( m < n ) then
    go to 340
  else if ( ldfjac < n ) then
    go to 340
  else if ( ftol < 0.0D+00 ) then
    go to 340
  else if ( xtol < 0.0D+00 ) then
    go to 340
  else if ( gtol < 0.0D+00 ) then
    go to 340
  else if ( maxfev <= 0 ) then
    go to 340
  else if ( factor <= 0.0D+00 ) then
    go to 340
  end if

  if ( mode == 2 ) then
    do j = 1, n
      if ( diag(j) <= 0.0D+00 ) then
        go to 340
      end if
    end do
  end if



  iflag = 1
  call fcn ( m, n, x, fvec, wa3, iflag )
  nfev = 1

  if ( iflag < 0 ) then
    go to 340
  end if

  fnorm = enorm ( m, fvec )



  par = 0.0D+00
  iter = 1



   30 continue



     if ( 0 < nprint ) then
       iflag = 0
       if ( mod ( iter-1, nprint ) == 0 ) then
         call fcn ( m, n, x, fvec, wa3, iflag )
       end if
       if ( iflag < 0 ) then
         go to 340
       end if
     end if





     qtf(1:n) = 0.0D+00
     fjac(1:n,1:n) = 0.0D+00
     iflag = 2

     do i = 1, m
       call fcn ( m, n, x, fvec, wa3, iflag )
       if ( iflag < 0 ) then
         go to 340
       end if
       temp = fvec(i)
       call rwupdt ( n, fjac, ldfjac, wa3, qtf, temp, wa1, wa2 )
       iflag = iflag + 1
     end do

     njev = njev + 1




     sing = .false.
     do j = 1, n
       if ( fjac(j,j) == 0.0D+00 ) then
         sing = .true.
       end if
       ipvt(j) = j
       wa2(j) = enorm ( j, fjac(1,j) )
     end do

     if ( sing ) then

       pivot = .true.
       call qrfac ( n, n, fjac, ldfjac, pivot, ipvt, n, wa1, wa2 )

       do j = 1, n

         if ( fjac(j,j) /= 0.0D+00 ) then

           sum2 = dot_product ( qtf(j:n), fjac(j:n,j) )
           temp = - sum2 / fjac(j,j)
           qtf(j:n) = qtf(j:n) + fjac(j:n,j) * temp

         end if

         fjac(j,j) = wa1(j)

       end do

     end if







     if ( iter == 1 ) then

       if ( mode /= 2 ) then

         diag(1:n) = wa2(1:n)
         do j = 1, n
           if ( wa2(j) == 0.0D+00 ) then
             diag(j) = 1.0D+00
           end if
         end do

       end if

       wa3(1:n) = diag(1:n) * x(1:n)
       xnorm = enorm ( n, wa3 )
       delta = factor * xnorm
       if ( delta == 0.0D+00 ) then
         delta = factor
       end if

     end if



     gnorm = 0.0D+00

     if ( fnorm /= 0.0D+00 ) then

       do j = 1, n
         l = ipvt(j)
         if ( wa2(l) /= 0.0D+00 ) then
           sum2 = dot_product ( qtf(1:j), fjac(1:j,j) ) / fnorm
           gnorm = max ( gnorm, abs ( sum2 / wa2(l) ) )
         end if
       end do

     end if



     if ( gnorm <= gtol ) then
       info = 4
       go to 340
     end if



     if ( mode /= 2 ) then
       do j = 1, n
         diag(j) = max ( diag(j), wa2(j) )
       end do
     end if



240    continue



        call lmpar ( n, fjac, ldfjac, ipvt, diag, qtf, delta, par, wa1, wa2 )




        wa1(1:n) = -wa1(1:n)
        wa2(1:n) = x(1:n) + wa1(1:n)
        wa3(1:n) = diag(1:n) * wa1(1:n)
        pnorm = enorm ( n, wa3 )



        if ( iter == 1 ) then
          delta = min ( delta, pnorm )
        end if



        iflag = 1
        call fcn ( m, n, wa2, wa4, wa3, iflag )
        nfev = nfev + 1
        if ( iflag < 0 ) then
          go to 340
        end if
        fnorm1 = enorm ( m, wa4 )



        if ( 0.1D+00 * fnorm1 < fnorm ) then
          actred = 1.0D+00 - ( fnorm1 / fnorm ) ** 2
        else
          actred = -1.0D+00
        end if




        do j = 1, n
          wa3(j) = 0.0D+00
          l = ipvt(j)
          temp = wa1(l)
          wa3(1:j) = wa3(1:j) + fjac(1:j,j) * temp
        end do

        temp1 = enorm ( n, wa3 ) / fnorm
        temp2 = ( sqrt(par) * pnorm ) / fnorm
        prered = temp1 ** 2 + temp2 ** 2 / 0.5D+00
        dirder = - ( temp1 ** 2 + temp2 ** 2 )



        if ( prered /= 0.0D+00 ) then
          ratio = actred / prered
        else
          ratio = 0.0D+00
        end if



        if ( ratio <= 0.25D+00 ) then

          if ( actred >= 0.0D+00 ) then
            temp = 0.5D+00
          else
            temp = 0.5D+00 * dirder / ( dirder + 0.5D+00 * actred )
          end if

          if ( 0.1D+00 * fnorm1 >= fnorm .or. temp < 0.1D+00 ) then
            temp = 0.1D+00
          end if

          delta = temp * min ( delta, pnorm / 0.1D+00 )
          par = par / temp

        else

          if ( par == 0.0D+00 .or. ratio >= 0.75D+00 ) then
            delta = pnorm / 0.5D+00
            par = 0.5D+00 * par
          end if

        end if



        if ( ratio >= 0.0001D+00 ) then
          x(1:n) = wa2(1:n)
          wa2(1:n) = diag(1:n) * x(1:n)
          fvec(1:m) = wa4(1:m)
          xnorm = enorm ( n, wa2 )
          fnorm = fnorm1
          iter = iter + 1
        end if



        if ( abs ( actred ) <= ftol .and. prered <= ftol &
          .and. 0.5D+00 * ratio <= 1.0D+00 ) then
          info = 1
        end if

        if ( delta <= xtol * xnorm ) then
          info = 2
        end if

        if ( abs ( actred ) <= ftol .and. prered <= ftol &
          .and. 0.5D+00 * ratio <= 1.0D+00 .and. info == 2 ) then
          info = 3
        end if

        if ( info /= 0 ) then
          go to 340
        end if

        if ( nfev >= maxfev) then
          info = 5
        end if

        if ( abs ( actred ) <= epsmch .and. prered <= epsmch &
          .and. 0.5D+00 * ratio <= 1.0D+00 ) then
          info = 6
        end if

        if ( delta <= epsmch * xnorm ) then
          info = 7
        end if

        if ( gnorm <= epsmch ) then
          info = 8
        end if

        if ( info /= 0 ) then
          go to 340
        end if



        if ( ratio < 0.0001D+00 ) then
          go to 240
        end if



     go to 30

  340 continue



  if ( iflag < 0 ) then
    info = iflag
  end if

  iflag = 0

  if ( 0 < nprint ) then
    call fcn ( m, n, x, fvec, wa3, iflag )
  end if

  return
end subroutine lmstr
subroutine lmstr1 ( fcn, m, n, x, fvec, fjac, ldfjac, tol, info )




































































































  implicit none

  integer ( kind = 4 ) ldfjac
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) diag(n)
  real ( kind = 8 ) factor
  external fcn
  real ( kind = 8 ) fjac(ldfjac,n)
  real ( kind = 8 ) ftol
  real ( kind = 8 ) fvec(m)
  real ( kind = 8 ) gtol
  integer ( kind = 4 ) info
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) maxfev
  integer ( kind = 4 ) mode
  integer ( kind = 4 ) nfev
  integer ( kind = 4 ) njev
  integer ( kind = 4 ) nprint
  real ( kind = 8 ) qtf(n)
  real ( kind = 8 ) tol
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) xtol

  if ( n <= 0 ) then
    info = 0
    return
  end if

  if ( m < n ) then
    info = 0
    return
  end if

  if ( ldfjac < n ) then
    info = 0
    return
  end if

  if ( tol < 0.0D+00 ) then
    info = 0
    return
  end if

  fvec(1:n) = 0.0D+00
  fjac(1:ldfjac,1:n) = 0.0D+00
  ftol = tol
  xtol = tol
  gtol = 0.0D+00
  maxfev = 100 * ( n + 1 )
  diag(1:n) = 0.0D+00
  mode = 1
  factor = 100.0D+00
  nprint = 0
  info = 0
  nfev = 0
  njev = 0
  ipvt(1:n) = 0
  qtf(1:n) = 0.0D+00

  call lmstr ( fcn, m, n, x, fvec, fjac, ldfjac, ftol, xtol, gtol, maxfev, &
    diag, mode, factor, nprint, info, nfev, njev, ipvt, qtf )

  if ( info == 8 ) then
    info = 4
  end if

  return
end subroutine lmstr1
subroutine qform ( m, n, q, ldq )
















































  implicit none

  integer ( kind = 4 ) ldq
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) minmn
  real ( kind = 8 ) q(ldq,m)
  real ( kind = 8 ) temp
  real ( kind = 8 ) wa(m)

  minmn = min ( m, n )

  do j = 2, minmn
    q(1:j-1,j) = 0.0D+00
  end do



  q(1:m,n+1:m) = 0.0D+00

  do j = n+1, m
    q(j,j) = 1.0D+00
  end do



  do l = 1, minmn

    k = minmn - l + 1

    wa(k:m) = q(k:m,k)

    q(k:m,k) = 0.0D+00
    q(k,k) = 1.0D+00

    if ( wa(k) /= 0.0D+00 ) then

      do j = k, m
        temp = dot_product ( wa(k:m), q(k:m,j) ) / wa(k)
        q(k:m,j) = q(k:m,j) - temp * wa(k:m)
      end do

    end if

  end do

  return
end subroutine qform
subroutine qrfac ( m, n, a, lda, pivot, ipvt, lipvt, rdiag, acnorm )








































































  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) lipvt
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(lda,n)
  real ( kind = 8 ) acnorm(n)
  real ( kind = 8 ) ajnorm
  real ( kind = 8 ) enorm
  real ( kind = 8 ) epsmch
  
  integer ( kind = 4 ) i4_temp
  integer ( kind = 4 ) ipvt(lipvt)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kmax
  integer ( kind = 4 ) minmn
  logical pivot
  real ( kind = 8 ) r8_temp(m)
  real ( kind = 8 ) rdiag(n)
  real ( kind = 8 ) temp
  real ( kind = 8 ) wa(n)

  epsmch = epsilon ( epsmch )



  do j = 1, n
    acnorm(j) = enorm ( m, a(1:m,j) )
  end do

  rdiag(1:n) = acnorm(1:n)
  wa(1:n) = acnorm(1:n)

  if ( pivot ) then
    do j = 1, n
      ipvt(j) = j
    end do
  end if



  minmn = min ( m, n )

  do j = 1, minmn



    if ( pivot ) then

      kmax = j

      do k = j, n
        if ( rdiag(kmax) < rdiag(k) ) then
          kmax = k
        end if
      end do

      if ( kmax /= j ) then

        r8_temp(1:m) = a(1:m,j)
        a(1:m,j)     = a(1:m,kmax)
        a(1:m,kmax)  = r8_temp(1:m)

        rdiag(kmax) = rdiag(j)
        wa(kmax) = wa(j)

        i4_temp    = ipvt(j)
        ipvt(j)    = ipvt(kmax)
        ipvt(kmax) = i4_temp

      end if

    end if




    ajnorm = enorm ( m-j+1, a(j,j) )

    if ( ajnorm /= 0.0D+00 ) then

      if ( a(j,j) < 0.0D+00 ) then
        ajnorm = -ajnorm
      end if

      a(j:m,j) = a(j:m,j) / ajnorm
      a(j,j) = a(j,j) + 1.0D+00



      do k = j + 1, n

        temp = dot_product ( a(j:m,j), a(j:m,k) ) / a(j,j)

        a(j:m,k) = a(j:m,k) - temp * a(j:m,j)

        if ( pivot .and. rdiag(k) /= 0.0D+00 ) then

          temp = a(j,k) / rdiag(k)
          rdiag(k) = rdiag(k) * sqrt ( max ( 0.0D+00, 1.0D+00-temp ** 2 ) )

          if ( 0.05D+00 * ( rdiag(k) / wa(k) ) ** 2 <= epsmch ) then
            rdiag(k) = enorm ( m-j, a(j+1,k) )
            wa(k) = rdiag(k)
          end if

        end if

      end do

    end if

    rdiag(j) = - ajnorm

  end do

  return
end subroutine qrfac
subroutine qrsolv ( n, r, ldr, ipvt, diag, qtb, x, sdiag )



















































































  implicit none

  integer ( kind = 4 ) ldr
  integer ( kind = 4 ) n

  real ( kind = 8 ) c
  real ( kind = 8 ) cotan
  real ( kind = 8 ) diag(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ipvt(n)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) nsing
  real ( kind = 8 ) qtb(n)
  real ( kind = 8 ) qtbpj
  real ( kind = 8 ) r(ldr,n)
  real ( kind = 8 ) s
  real ( kind = 8 ) sdiag(n)
  real ( kind = 8 ) sum2
  real ( kind = 8 ) t
  real ( kind = 8 ) temp
  real ( kind = 8 ) wa(n)
  real ( kind = 8 ) x(n)





  do j = 1, n
    r(j:n,j) = r(j,j:n)
    x(j) = r(j,j)
  end do

  wa(1:n) = qtb(1:n)



  do j = 1, n




    l = ipvt(j)

    if ( diag(l) /= 0.0D+00 ) then

      sdiag(j:n) = 0.0D+00
      sdiag(j) = diag(l)





      qtbpj = 0.0D+00

      do k = j, n




        if ( sdiag(k) /= 0.0D+00 ) then

          if ( abs ( r(k,k) ) < abs ( sdiag(k) ) ) then
            cotan = r(k,k) / sdiag(k)
            s = 0.5D+00 / sqrt ( 0.25D+00 + 0.25D+00 * cotan ** 2 )
            c = s * cotan
          else
            t = sdiag(k) / r(k,k)
            c = 0.5D+00 / sqrt ( 0.25D+00 + 0.25D+00 * t ** 2 )
            s = c * t
          end if




          r(k,k) = c * r(k,k) + s * sdiag(k)
          temp = c * wa(k) + s * qtbpj
          qtbpj = - s * wa(k) + c * qtbpj
          wa(k) = temp



          do i = k+1, n
            temp = c * r(i,k) + s * sdiag(i)
            sdiag(i) = - s * r(i,k) + c * sdiag(i)
            r(i,k) = temp
          end do

        end if

      end do

    end if




    sdiag(j) = r(j,j)
    r(j,j) = x(j)

  end do




  nsing = n

  do j = 1, n

    if ( sdiag(j) == 0.0D+00 .and. nsing == n ) then
      nsing = j - 1
    end if

    if ( nsing < n ) then
      wa(j) = 0.0D+00
    end if

  end do

  do j = nsing, 1, -1
    sum2 = dot_product ( wa(j+1:nsing), r(j+1:nsing,j) )
    wa(j) = ( wa(j) - sum2 ) / sdiag(j)
  end do



  do j = 1, n
    l = ipvt(j)
    x(l) = wa(j)
  end do

  return
end subroutine qrsolv
subroutine r1mpyq ( m, n, a, lda, v, w )





















































  implicit none

  integer ( kind = 4 ) lda
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(lda,n)
  real ( kind = 8 ) c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) s
  real ( kind = 8 ) temp
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) w(n)



  do j = n - 1, 1, -1

     if ( 1.0D+00 < abs ( v(j) ) ) then
       c = 1.0D+00 / v(j)
       s = sqrt ( 1.0D+00 - c ** 2 )
     else
       s = v(j)
       c = sqrt ( 1.0D+00 - s ** 2 )
     end if

     do i = 1, m
        temp =   c * a(i,j) - s * a(i,n)
        a(i,n) = s * a(i,j) + c * a(i,n)
        a(i,j) = temp
     end do

  end do



  do j = 1, n - 1

     if ( abs ( w(j) ) > 1.0D+00 ) then
       c = 1.0D+00 / w(j)
       s = sqrt ( 1.0D+00 - c ** 2 )
     else
       s = w(j)
       c = sqrt ( 1.0D+00 - s ** 2 )
     end if

     do i = 1, m
        temp =     c * a(i,j) + s * a(i,n)
        a(i,n) = - s * a(i,j) + c * a(i,n)
        a(i,j) = temp
     end do

  end do

  return
end subroutine r1mpyq
subroutine r1updt ( m, n, s, ls, u, v, w, sing )






































































  implicit none

  integer ( kind = 4 ) ls
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) cos
  real ( kind = 8 ) cotan
  real ( kind = 8 ) giant
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jj
  integer ( kind = 4 ) l
  real ( kind = 8 ) s(ls)
  real ( kind = 8 ) sin
  logical sing
  real ( kind = 8 ) tan
  real ( kind = 8 ) tau
  real ( kind = 8 ) temp
  real ( kind = 8 ) u(m)
  real ( kind = 8 ) v(n)
  real ( kind = 8 ) w(m)



  giant = huge ( giant )



  jj = ( n * ( 2 * m - n + 1 ) ) / 2 - ( m - n )



  l = jj
  do i = n, m
    w(i) = s(l)
    l = l + 1
  end do




  do j = n - 1, 1, -1

    jj = jj - ( m - j + 1 )
    w(j) = 0.0D+00

    if ( v(j) /= 0.0D+00 ) then




      if ( abs ( v(n) ) < abs ( v(j) ) ) then
        cotan = v(n) / v(j)
        sin = 0.5D+00 / sqrt ( 0.25D+00 + 0.25D+00 * cotan ** 2 )
        cos = sin * cotan
        tau = 1.0D+00
        if ( abs ( cos ) * giant > 1.0D+00 ) then
          tau = 1.0D+00 / cos
        end if
      else
        tan = v(j) / v(n)
        cos = 0.5D+00 / sqrt ( 0.25D+00 + 0.25D+00 * tan ** 2 )
        sin = cos * tan
        tau = sin
      end if




      v(n) = sin * v(j) + cos * v(n)
      v(j) = tau



      l = jj
      do i = j, m
        temp = cos * s(l) - sin * w(i)
        w(i) = sin * s(l) + cos * w(i)
        s(l) = temp
        l = l + 1
      end do

    end if

  end do



   w(1:m) = w(1:m) + v(n) * u(1:m)



  sing = .false.

  do j = 1, n-1

    if ( w(j) /= 0.0D+00 ) then




      if ( abs ( s(jj) ) < abs ( w(j) ) ) then

        cotan = s(jj) / w(j)
        sin = 0.5D+00 / sqrt ( 0.25D+00 + 0.25D+00 * cotan ** 2 )
        cos = sin * cotan

        if ( 1.0D+00 < abs ( cos ) * giant ) then
          tau = 1.0D+00 / cos
        else
          tau = 1.0D+00
        end if

      else

        tan = w(j) / s(jj)
        cos = 0.5D+00 / sqrt ( 0.25D+00 + 0.25D+00 * tan ** 2 )
        sin = cos * tan
        tau = sin

      end if



      l = jj
      do i = j, m
        temp = cos * s(l) + sin * w(i)
        w(i) = - sin * s(l) + cos * w(i)
        s(l) = temp
        l = l + 1
      end do



      w(j) = tau

    end if



    if ( s(jj) == 0.0D+00 ) then
      sing = .true.
    end if

    jj = jj + ( m - j + 1 )

  end do



  l = jj
  do i = n, m
    s(l) = w(i)
    l = l + 1
  end do

  if ( s(jj) == 0.0D+00 ) then
    sing = .true.
  end if

  return
end subroutine r1updt
subroutine r8vec_print ( n, a, title )





























  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '
  do i = 1, n
    write ( *, '(2x,i8,2x,g16.8)' ) i, a(i)
  end do

  return
end subroutine r8vec_print
subroutine rwupdt ( n, r, ldr, w, b, alpha, c, s )

































































  implicit none

  integer ( kind = 4 ) ldr
  integer ( kind = 4 ) n

  real ( kind = 8 ) alpha
  real ( kind = 8 ) b(n)
  real ( kind = 8 ) c(n)
  real ( kind = 8 ) cotan
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) r(ldr,n)
  real ( kind = 8 ) rowj
  real ( kind = 8 ) s(n)
  real ( kind = 8 ) tan
  real ( kind = 8 ) temp
  real ( kind = 8 ) w(n)

  do j = 1, n

    rowj = w(j)



    do i = 1, j - 1
      temp =   c(i) * r(i,j) + s(i) * rowj
      rowj = - s(i) * r(i,j) + c(i) * rowj
      r(i,j) = temp
    end do



    c(j) = 1.0D+00
    s(j) = 0.0D+00

    if ( rowj /= 0.0D+00 ) then

      if ( abs ( r(j,j) ) < abs ( rowj ) ) then
        cotan = r(j,j) / rowj
        s(j) = 0.5D+00 / sqrt ( 0.25D+00 + 0.25D+00 * cotan ** 2 )
        c(j) = s(j) * cotan
      else
        tan = rowj / r(j,j)
        c(j) = 0.5D+00 / sqrt ( 0.25D+00 + 0.25D+00 * tan ** 2 )
        s(j) = c(j) * tan
      end if



      r(j,j) =  c(j) * r(j,j) + s(j) * rowj
      temp =    c(j) * b(j)   + s(j) * alpha
      alpha = - s(j) * b(j)   + c(j) * alpha
      b(j) = temp

    end if

  end do

  return
end subroutine rwupdt
subroutine timestamp ( )

























  implicit none

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end subroutine timestamp
