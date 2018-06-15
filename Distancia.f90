PROGRAM Mapa_Logistico

!VARIAVEIS!

	!Constantes!
	REAL(8), PARAMETER :: r = 2.5 !Taxa de crescimento!
	REAL(8), PARAMETER :: eps = 1e-3 !Epsilon!
    
	!Razoes iniciais!
	REAL(8) :: v = 0.1d0 
	REAL(8) :: w = 0.3d0
	REAL(8) :: x = 0.5d0 
	REAL(8) :: y = 0.7d0
	REAL(8) :: z = 0.9d0
	
	!Razoes barra iniciais!
	REAL(8) :: bv, bw, bx, by, bz

	!Diferencas!
	REAL(8) :: dv, dw, dx, dy, dz	

!VARIAVEIS DE TRABALHO!

    INTEGER :: i

!CONDICOES INICIAIS!

	!Razoes barra iniciais!
	bv = v + eps 
	bw = w + eps 
	bx = x + eps 
	by = y + eps
	bz = z + eps 

	!Diferencas!
	dv = bv - v 
	dw = bw - w 
	dx = bx - x 
	dy = by - y
	dz = bz - z

!FORMATACAO E OUTPUT!

	100 FORMAT (' ', i5, ' ', f20.15, f20.15, f20.15, f20.15, f20.15)
	OPEN(10, FILE = 'distancia.dat')

	WRITE(10,100) i, dv, dw, dx, dy, dz

!ITERACAO!

	DO i = 1, 50 !Geracoes!

		!Recalculo das razoes!
		v = r*v*(1 - v)
		w = r*w*(1 - w)
		x = r*x*(1 - x)
		y = r*y*(1 - y)
		z = r*z*(1 - z)

		!Recalculo das barra razoes!
		bv = r*bv*(1 - bv)
		bw = r*bw*(1 - bw)
		bx = r*bx*(1 - bx)
		by = r*by*(1 - by)
		bz = r*bz*(1 - bz)

		!Recalculo das diferencas!
		dv = ABS(bv - v) 
		dw = ABS(bw - w)
		dx = ABS(bx - x)
		dy = ABS(by - y)
		dz = ABS(bz - z)

		!Output!
		WRITE(10,100) i, dv, dw, dx, dy, dz

	END DO

	CLOSE(10)

!PLOTAGEM!
	
	CALL system ('gnuplot plotdistancia.gnu')
	CALL system ('display distancia.png')	

END PROGRAM
