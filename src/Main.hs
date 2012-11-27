{--
        Esqueleto de programa para geração de bubble cloud em Haskell.
        Mais informações em: http://www.inf.ufsm.br/~andrea/elc117-2012b
--}


module Main where

import Text.Printf --
import System.Random --random
import System.IO.Unsafe 

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Circle    = (Point,Float)

imageWidth :: Int
imageWidth = 360

imageHeight :: Int
imageHeight = 360


-- Funcao principal que faz leitura do dataset e gera arquivo SVG
main :: IO ()
main = do 
        strcontent <- readFile infile
        let pairs = map (span (/= ' ')) (lines strcontent)
            freqs = readInts (map snd pairs)
            tags = (map fst pairs)
        writeFile outfile (svgCloudGen imageWidth imageHeight freqs)
        putStrLn "Ok!"
        where 
                infile = "dataset.txt"
                outfile = "tagcloud.svg"


-- Transforma lista de strings em lista de inteiros
readInts :: [String] -> [Int]
readInts ss = map read ss


-- Gera o documento SVG da tag cloud, concatenando cabecalho, conteudo e rodape
svgCloudGen :: Int -> Int -> [Int] -> String
svgCloudGen w h dataset = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
        (svgViewBox w h) ++
        (concat (svgBubbleGen w h dataset)) ++ "</svg>\n"


-- Esta funcao deve gerar a lista de circulos em formato SVG.
-- A implementacao atual eh apenas um teste que gera um circulo posicionado no meio da figura.
-- TODO: Alterar essa funcao para usar os dados do dataset.
svgBubbleGen:: Int -> Int -> [Int] -> [String]
svgBubbleGen w h dataset = 
                let 
                raios = geraRaio dataset -- gera lista com raios (float)
                pontos = geraPoints (fromIntegral w/2) (fromIntegral h/2) raios 0 -- gera pontos para a espiral
                circulos = insereRaios raios -- gera circulos com x=180,y=180 e raio= freq/100
                in map svgCircle (inserePoints pontos circulos)


-- Gera string representando um circulo em SVG. A cor do circulo esta fixa. 
-- TODO: Alterar esta funcao para mostrar um circulo de uma cor fornecida como parametro.
svgCircle :: Circle -> String
svgCircle ((x,y),r) = 
            let colorR = unsafePerformIO geraRandomico 
                colorG = unsafePerformIO geraRandomico
                colorB = unsafePerformIO geraRandomico
            in printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(%d,%d,%d)\" />\n" x y r colorR colorG colorB



-- Configura o viewBox da imagem e coloca retangulo branco no fundo
svgViewBox :: Int -> Int -> String
svgViewBox w h =
        printf  "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:white;\"/>\n" w h



geraPoints :: Float -> Float -> [Float] -> Float -> [Point]
geraPoints _ _ [] _ = []
geraPoints _ _ [v] _ = []
geraPoints w h (x:xs) n = 
    let a = 1 
        t = n + 0.1
        cX = verificaPonto (w + (a * t * (cos t))) 
        cY = verificaPonto (h + (a * t * (sin t))) 
    in (cX,cY) : (geraPoints cX cY xs t )
    
verificaPonto :: Float -> Float
verificaPonto x
    |x > 360 = x-360
    |otherwise = x

distpontos :: Point -> Point -> Float
distpontos (x1,y1) (x2,y2) =
    let dx = (x2-x1)^2
        dy = (y2-y1)^2
        dxy = dx + dy 
    in sqrt dxy

-- funcao que ordena uma lista decrescente
ordena :: [Int] -> [Int]
ordena [] = []
ordena (a:x) = aux a (ordena x)
                
-- funcao auxiliar para ordena��o
aux :: Int -> [Int] -> [Int]
aux a [] = [a]
aux a (b:x)
    | a >= b = a : (b:x)
    | otherwise = b : aux a x

geraRaio :: [Int] -> [Float]
geraRaio x = map (*1.3) a
        where
        b = map (fromIntegral) (x)
        a = map (sqrt) b

geraRandomico :: IO Int
geraRandomico = getStdRandom (randomR (0,255))

-- recebe uma lista com raios e retorna uma lista de circulos
insereRaios :: [Float] -> [Circle] 
insereRaios [] = []
insereRaios (x:xs) = ((fromIntegral imageWidth/2, fromIntegral imageHeight/2), x) : insereRaios xs

inserePoints :: [Point] -> [Circle] -> [Circle]
inserePoints [] _ = []
inserePoints _ [] = []
inserePoints ((x,y):xs) (((cx,cy),r):ys) = ((x,y),r) : inserePoints xs ys
