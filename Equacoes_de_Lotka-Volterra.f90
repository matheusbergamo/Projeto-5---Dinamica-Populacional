PROGRAM Equacoes_de_Lotka-Volterra

!VARIAVEIS!

	REAL(8) :: t !Tempo!
	REAL(8) :: x !Numero de presas!
	REAL(8) :: y !Numero de predadores!

!CONSTANTES!

	REAL(8), PARAMETER :: a = 2.0d0/3.0d0
	REAL(8), PARAMETER :: b = 4.0d0/3.0d0
	REAL(8), PARAMETER :: c = 1.0d0
	REAL(8), PARAMETER :: d = 1.0d0

!VARIAVEIS E CONSTANTES DE TRABALHO!

	INTEGER :: i
	REAL(8), PARAMETER :: dt = 1e-4

!CONDICOES INICIAIS!

	!Tempo!
	t = 0

	!Numero de presas!
	x = 1

	!Numero de predadores!
	y = 1

!FORMATACAO E OUTPUT!

	100 FORMAT (' ', f10.4, f10.6, f10.6)
	OPEN(10, FILE = 'lotka.dat') 

	WRITE(10,100) t, x, y

!METODO DE EULER-CROMER!

	DO i = 1, 2000000

		!Recalculo do tempo!
		t = i*dt
	
		!Recalculo do numero de presas!
		x = x + (a*x - b*x*y)*dt
	
		!Recalculo do numero de predadores!
		y = y + (-c*y + d*x*y)*dt
	
		!Output!
		WRITE(10,100) t, x, y

	END DO	

!PLOTAGEM!

	CALL system ('gnuplot lotka.gnu')
	CALL system ('display lotka.png')

END PROGRAM
