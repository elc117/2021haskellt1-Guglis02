import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)

-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Função que escolhe as cores da imagem
-- Caso o input do usuário seja um número aleatório, a opção selecionada será a última
choosePalette:: Int -> Int -> [(Int,Int,Int)]
choosePalette x n
 | x == 0 = creeperPalette n
 | x == 1 = zombiePalette n
 | otherwise = skeletonPalette n

-- Paleta para o Creeper
creeperPalette :: Int -> [(Int,Int,Int)]
creeperPalette n = take n $ cycle [(143, 227, 143),(15, 128, 15),(13, 181, 13)]

-- Paleta para o Zumbi
zombiePalette :: Int -> [(Int,Int,Int)]
zombiePalette n = take n $ cycle [(74, 111, 40),(91, 135, 49),(82, 122, 45),(91, 139, 50),(91, 139, 50)]

-- Paleta para o Esqueleto
skeletonPalette :: Int -> [(Int,Int,Int)]
skeletonPalette n = take n $ cycle [(176, 173, 173),(189, 189, 189),(168, 168, 168)]

-------------------------------------------------------------------------------
-- Geração de retângulos em suas posições
-------------------------------------------------------------------------------

genRectsInLine :: Int -> Float -> Float -> [Rect]
genRectsInLine n x y = [((m*w, p), w, h) | m <- [0..fromIntegral (n-1)], p <- [0,h..h*7]]
  where (w,h) = (x/8,y/8)

-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

-------------------------------------------------------------------------------
-- Funções para gerar os rostos
-------------------------------------------------------------------------------

-- Função que escolhe a figura que será desenhada
-- Caso o input do usuário seja um número aleatório, a opção selecionada será a última
chooseFace :: Int -> Float -> Float -> String
chooseFace x w h
 | x == 0 = svgCreeperFace w h
 | x == 1 = svgZombieFace w h
 | otherwise = svgSkeletonFace w h

svgCreeperFace :: Float -> Float -> String
svgCreeperFace w h = 
  -- Olhos
  svgRect ((w/8,h/4),w/4,h/4) color ++
  svgRect ((w/8*5,h/4),w/4,h/4) color ++
  -- Boca
  svgRect ((w/4,h/8*5),w/2,h/4) color ++
  svgRect ((w/8*3,h/2),w/4,h/4) color ++
  svgRect ((w/4,h/8*7),w/8,h/8) color ++
  svgRect ((w/8*5,h/8*7),w/8,h/8) color
  where 
    color = "fill:rgb(1,1,1);"

svgZombieFace :: Float -> Float -> String
svgZombieFace w h = 
  -- Olhos
  svgRect ((w/8,h/2),w/4,h/8) color ++
  svgRect ((w/8*5,h/2),w/4,h/8) color ++
  -- Nariz
  svgRect ((w/8*3,h/8*5),w/4,h/8) color2 ++
  -- Boca
  svgRect ((w/4,h/4*3),w/8,h/4) color2 ++
  svgRect ((w/8*5,h/4*3),w/8,h/4) color2
  where 
    color = "fill:rgb(1,1,1);"
    color2 = "fill:rgb(1,1,1,0.7);"

svgSkeletonFace :: Float -> Float -> String
svgSkeletonFace w h = 
  -- Olhos
  svgRect ((w/8,h/2),w/4,h/8) color ++
  svgRect ((w/8*5,h/2),w/4,h/8) color ++
  -- Nariz
  svgRect ((w/8*3,h/8*5),w/4,h/8) color2 ++
  -- Boca
  svgRect ((w/8,h/4*3),w/4*3,h/8) color
  where 
    color = "fill:rgb(66, 66, 66);" 
    color2 = "fill:rgb(117, 117, 117);"

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

gerar :: Float -> Float -> Int -> Int -> IO ()
-- option1 decide a paleta de cores
-- option2 decide a figura
gerar w h option1 option2 = do
  writeFile "mob.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ face ++ svgEnd
        svgfigs = svgElements svgRect rects (map svgStyle palette)
        rects = genRectsInLine nrects w h
        palette = choosePalette option1 nrects
        face = chooseFace option2 w h
        nrects = 64 
        

