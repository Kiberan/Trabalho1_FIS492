program AreaMC
!-----------------------------INTRODUÇÃO-----------------------------------
!   Author: Murilo Garcia
!   Date: 22/11/2024
!   email:murilo.garcia@ufv.br    

    !O programa consiste em encontrar uma aproximação para pi utilizando
    !a técnica de Monte Carlo via razão de áreas.
    !Para isso utilizamos do fato de que a razão entre a área de uma
    !circunferência inscrita em um quadrado de lado unitário é igual
    !a 1/4 de pi. Nesse sentido o algoritmo de Monte Carlo nos dará
    !quantos pontos existem dentro da circunferência (uma espécie de área
    !infinitesimal) quando o número de pontos totais (do quadrado) é previamente
    !estabelecido
!----------------------------------------------------------------------------------
    
    !Definindo nome para as precisões e adicionando modulo iso_fortran_env
    !que nos permite utilizar o rng padronizado do Fortran
    use rndgen_mod
    use, intrinsic :: iso_fortran_env, only: sp => real32, dp => real64, i4 => int32, i8 => int64
    implicit none
    integer(kind=i4) :: seed = 123456
    type(rndgen) :: generator
    real(kind=dp) :: x
    real(kind=dp) :: y  !Coordenadas (x,y) de um ponto do Quadrado
    integer(kind=i8) :: ciclos = 10000  !Ciclos corresponderá a quantidade de pontos
                                !totais para o cálculo da aproximação
                                !Será nossa variável auxiliar para o laço
                                !de repetição
    real(kind=dp) :: dentro = 0    !Responsável pela contagem de pontos dentro
                                !da circunferência
    real(kind=dp) :: razao      !Talvez eu use essa variável para a lógica
    integer(kind=i8) :: k       !Variável auxiliar para o Laço de Repetição
    call generator%init(seed)
                            !Laço de Repetição:
    do k=1, ciclos
        x = generator%rnd()
        y = generator%rnd()
        if (x**2 + y**2 < 1) then
            dentro = dentro + 1
        end if
    end do
    write(*,*) "O valor aproximado para π foi:" , 4* real(dentro,dp)/real(ciclos,dp)
end program