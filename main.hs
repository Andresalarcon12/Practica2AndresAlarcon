-- Hecho por Andrés Felipe Alarcón 23/05/2024
import Data.List (nub)

-- Definimos los tipos de programas académicos
data CategoriaPrograma = Administrative | Humanities | Engineering deriving (Show, Enum, Bounded)

-- Función para obtener los divisores propios de un número
divisoresPropios :: Int -> [Int]
divisoresPropios n = [x | x <- [1..(n `div` 2)], n `mod` x == 0]

-- Función para calcular la suma aliquot
sumaAliquota :: Int -> Int
sumaAliquota n = sum (divisoresPropios n)

-- Función para la clasificaxion de un número según Nicomachus
clasificacionNicomachus :: Int -> CategoriaPrograma
clasificacionNicomachus n
  | suma == n  = Engineering       -- número perfecto
  | suma > n   = Administrative   -- número abundante
  | otherwise  = Humanities      -- número deficiente
  where suma = sumaAliquota n

-- Función para convertir los primeros tres dígitos en el periodo
periodoToString :: Int -> String
periodoToString p = let year = 2000 + (p `div` 10)
                        semester = if odd p then "1" else "2"
                    in show year ++ "-" ++ semester

-- Función para saber si el numero es par o impar
esPar :: Int -> Bool
esPar num = num `mod` 2 == 0

-- Función principal para procesar el código de identificación
procesarCodigo :: Int -> String
procesarCodigo codigo = 
  let strCodigo = show codigo
      periodo = read (take 3 strCodigo) :: Int
      categoriaNum = read (take 2 (drop 3 strCodigo)) :: Int
      consecutivo = read (drop 5 strCodigo) :: Int
      categoria = clasificacionNicomachus categoriaNum
      periodoStr = periodoToString periodo
      parImpar = if esPar codigo then "even" else "odd"
  in periodoStr ++ " " ++ show categoria ++ " num" ++ show consecutivo ++ " " ++ parImpar

-- Función principal (la entrada del usuario)
main :: IO ()
main = do
  -- Leer entrada del usuario
 
  input <- getLine
  let codigo = read input :: Int
  -- Imprimir el resultado del procesamiento del código
  putStrLn (procesarCodigo codigo)


