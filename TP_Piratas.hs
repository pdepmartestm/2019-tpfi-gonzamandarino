type Botin= ([Char], Int)  -- Nombre, Valor

type Pirata= ([Char], [Botin], [Char]) -- Nombre, Botin, Saqueo

type Tripulacion= ([Char],[Pirata], [Char]) -- Nombre, Piratas, Saqueo

---------------GETS-----------------------------
get1st (a,_,_) = a

get2nd (_,a,_) = a

get3rd (_,_,a) = a

---------------TESOROS--------------------------


botines pirata = get2nd pirata

cantTesoros pirata = length (botines pirata)

afortunado pirata = sum (map (snd) (botines pirata))

--mismoTesoro pirata1 pirata2 = any 
loTiene pirata1 pirata2 = botines pirata1 == botines pirata2 

maximoTesoro pirata = maximum (botines pirata)

agregarTesoro pirata tesoro = (botines pirata) ++ tesoro

tesoroValioso pirata = filter (<100) (botines pirata)

damelo pirata = map (fst) (botines pirata)

eliminarTesoro pirata tesoro = delete tesoro (damelo pirata)

----------------------SAQUEOS----------------------

valiosos tesoro = filter (>100) tesoro 
nombres buscado tesoro = filter (==buscado) tesoro

SaquearSi funcion pirata = funcion botines pirata

SaquearValioso pirata = SaquearSi valiosos pirata
saquearTesorito pirata tesoro buscado
SaquearNombre buscado pirata = SaquearSi nombres pirata buscado
SaquearComplejo buscado pirata = SaquearValioso pirata || SaquearNombre buscado pirata

saquear pirata tesoro buscado
 | get3rd(pirata) == "Valioso" = SaquearValioso pirata
 | get3rd(pirata) == "Nombre" = SaquearNombre buscado pirata
 | get3rd(pirata) == "Complejo" = SaquearComplejo buscado pirata 