import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)


-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

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
genRectsInLine n x y = [((m*w, p), w, h) | m <- [0..fromIntegral (n-1)], p <- [0,y/8..y/8*7]]
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
-- Funções para gerar rostos
-------------------------------------------------------------------------------

svgCreeperFace :: Float -> Float -> String
svgCreeperFace w h = 
  -- Olhos
  svgRect ((w/8,h/4),w/4,h/4) "fill:rgb(1,1,1)" ++
  svgRect ((w/8*5,h/4),w/4,h/4) "fill:rgb(1,1,1)" ++
  -- Boca
  svgRect ((w/4,h/8*5),w/2,h/4) "fill:rgb(1,1,1)" ++
  svgRect ((w/8*3,h/2),w/4,h/4) "fill:rgb(1,1,1)" ++
  svgRect ((w/4,h/8*7),w/8,h/8) "fill:rgb(1,1,1)" ++
  svgRect ((w/8*5,h/8*7),w/8,h/8) "fill:rgb(1,1,1)"

svgZombieFace :: Float -> Float -> String
svgZombieFace w h = 
  -- Olhos
  svgRect ((w/8,h/2),w/4,h/8) "fill:rgb(1,1,1)" ++
  svgRect ((w/8*5,h/2),w/4,h/8) "fill:rgb(1,1,1)" ++
  -- Nariz
  svgRect ((w/8*3,h/8*5),w/4,h/8) "fill:rgb(60, 89, 31)" ++
  -- Boca
  svgRect ((w/4,h/4*3),w/8,h/4) "fill:rgb(60, 89, 31)" ++
  svgRect ((w/8*5,h/4*3),w/8,h/4) "fill:rgb(60, 89, 31)"

svgSkeletonFace :: Float -> Float -> String
svgSkeletonFace w h = 
  -- Olhos
  svgRect ((w/8,h/2),w/4,h/8) "fill:rgb(66, 66, 66)" ++
  svgRect ((w/8*5,h/2),w/4,h/8) "fill:rgb(66, 66, 66)" ++
  -- Nariz
  svgRect ((w/8*3,h/8*5),w/4,h/8) "fill:rgb(117, 117, 117)" ++
  -- Boca
  svgRect ((w/8,h/4*3),w/4*3,h/8) "fill:rgb(66, 66, 66)"
  
-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------


teste :: Float -> Float -> IO ()
teste w h = do
  writeFile "figs.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ face ++ svgEnd
        svgfigs = svgElements svgRect rects (map svgStyle palette)
        rects = genRectsInLine nrects w h
        palette = skeletonPalette nrects
        face = svgSkeletonFace w h
        nrects = 64
        --(w,h) = (800,800) -- width,height da imagem SVG



