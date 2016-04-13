module Project2 where



--necessary definitions

data RGB = RGB Int Int Int deriving (Show, Eq)
data Grid a = G [[a]] deriving (Show, Eq)
type Coord = (Int,Int)
type Picture = Grid RGB
type Path = [Coord]
type NodeEnergy = Int
type PathCost = Int
type Packet = (Coord, RGB, NodeEnergy) -- used much later
data Node = Node
	Coord -- this location
	RGB -- color info
	NodeEnergy -- energy at this spot
	PathCost -- cost of cheapest path from here to bottom.
	Node -- ref to next node on cheapest path. Nothing when we’re at the bottom.
	(Node,Node,Node) -- Three candidates we may connect to.
	| No -- sometimes there is no next node and we use No as a placeholder.
	deriving (Show, Eq)

--necessary definitions



gridLocs :: Grid Coord                                                                               
gridLocs = G [
	[(0,0),(0,1),(0,2)],
	[(1,0),(1,1),(1,2)],
	[(2,0),(2,1),(2,2)],
	[(3,0),(3,1),(3,2)]
	]

p1 :: Grid RGB
p1 = G [
	[RGB 100 75 200, RGB 100 100 200, RGB 100 100 200, RGB 100 100 200, RGB 200 125 200],
	[RGB 150 30 180, RGB 150 50 180, RGB 100 120 180, RGB 100 120 180, RGB 100 120 180],
	[RGB 100 75 100, RGB 100 80 100, RGB 100 85 100, RGB 100 95 100, RGB 100 110 100],
	[RGB 200 100 10, RGB 200 100 10, RGB 200 100 10, RGB 210 200 10, RGB 255 0 10]
	]

r :: Coord
r = (3,4)

r2 :: NodeEnergy
r2 = 500

r3 :: PathCost
r3 = 1000

r4 :: RGB
r4 = RGB 100 200 300

packetTest :: Packet
packetTest = (r, r4, r2) 

--nodeTest :: Node
--nodeTest = Node r (RGB 100 75 200) r2 r3 Nothing (No, No, No)

p :: RGB
p = RGB 4 5 6

k :: NodeEnergy
k = 0

j :: PathCost
j = 900

--m :: Node
--m = Node r p k j

--DONE
height :: Grid a -> Int
height (G a) = length a

width :: Grid a -> Int
width (G a) = 	widthHelper (head a)

widthHelper :: [a] -> Int
widthHelper a = length a

--let MyWidth = width
--Finds the energy of the given grid at the given coordinate. (Note, NodeEnergy is just a synonym for Int).
energyAt :: Grid RGB -> Coord -> NodeEnergy
energyAt a r
	| (fst r) < 1 && (snd r) < 1 =
			((abs(((getRed (getColumn (getRow a (fst r)) ((width a)-1)) - 
			(getRed (getColumn (getRow a (fst r)) ((snd r)+1))))^2)) +
			(abs(((getGreen (getColumn (getRow a (fst r)) ((width a)-1))) - 
			(getGreen (getColumn (getRow a (fst r)) ((snd r)+1))))^2)) +
			(abs(((getBlue (getColumn (getRow a (fst r)) ((width a)-1))) - 
			(getBlue (getColumn (getRow a (fst r)) ((snd r)+1))))^2))) +
			(abs(((getRed (getColumn (getRow a ((height a)-1)) (snd r)) - 
			(getRed (getColumn (getRow a ((fst r)+1)) (snd r))))^2)) +
			(abs(((getGreen (getColumn (getRow a ((height a)-1)) (snd r)) - 
			(getGreen (getColumn (getRow a ((fst r)+1)) (snd r))))^2)) +
			(abs(((getBlue (getColumn (getRow a ((height a)-1)) (snd r)) - 
			(getBlue (getColumn (getRow a ((fst r)+1)) (snd r))))^2))))))
	| (fst r) < 1 && (snd r) >= ((width a) -1) =
			((abs(((getRed (getColumn (getRow a (fst r)) ((snd r)-1)) - 
			(getRed (getColumn (getRow a (fst r)) 0 )))^2)) +
			(abs(((getGreen (getColumn (getRow a (fst r)) ((snd r)-1))) - 
			(getGreen (getColumn (getRow a (fst r)) 0 )))^2)) +
			(abs(((getBlue (getColumn (getRow a (fst r)) ((snd r)-1))) - 
			(getBlue (getColumn (getRow a (fst r)) 0 )))^2))) +
			(abs(((getRed (getColumn (getRow a ((height a)-1)) (snd r)) - 
			(getRed (getColumn (getRow a ((fst r)+1)) (snd r))))^2)) +
			(abs(((getGreen (getColumn (getRow a ((height a)-1)) (snd r)) - 
			(getGreen (getColumn (getRow a ((fst r)+1)) (snd r))))^2)) +
			(abs(((getBlue (getColumn (getRow a ((height a)-1)) (snd r)) - 
			(getBlue (getColumn (getRow a ((fst r)+1)) (snd r))))^2))))))
	| (fst r) >= ((height a) -1) && (snd r) < 1 =
			((abs(((getRed (getColumn (getRow a (fst r)) ((width a)-1)) - 
			(getRed (getColumn (getRow a (fst r)) ((snd r)+1))))^2)) +
			(abs(((getGreen (getColumn (getRow a (fst r)) ((width a)-1))) - 
			(getGreen (getColumn (getRow a (fst r)) ((snd r)+1))))^2)) +
			(abs(((getBlue (getColumn (getRow a (fst r)) ((width a)-1))) - 
			(getBlue (getColumn (getRow a (fst r)) ((snd r)+1))))^2))) +
			(abs(((getRed (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getRed (getColumn (getRow a 0) (snd r))))^2)) +
			(abs(((getGreen (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getGreen (getColumn (getRow a 0) (snd r))))^2)) +
			(abs(((getBlue (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getBlue (getColumn (getRow a 0) (snd r))))^2))))))
	| (fst r) >= ((height a)-1) && (snd r) >= ((width a)-1) =
			((abs(((getRed (getColumn (getRow a (fst r)) ((snd r)-1)) - 
			(getRed (getColumn (getRow a (fst r)) 0)))^2)) +
			(abs(((getGreen (getColumn (getRow a (fst r)) ((snd r)-1))) - 
			(getGreen (getColumn (getRow a (fst r)) 0)))^2)) +
			(abs(((getBlue (getColumn (getRow a (fst r)) ((snd r)-1))) - 
			(getBlue (getColumn (getRow a (fst r)) 0)))^2))) +
			(abs(((getRed (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getRed (getColumn (getRow a 0) (snd r))))^2)) +
			(abs(((getGreen (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getGreen (getColumn (getRow a 0) (snd r))))^2)) +
			(abs(((getBlue (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getBlue (getColumn (getRow a 0) (snd r))))^2))))))
	| (fst r) > 0 && (snd r) > 0 && (snd r) < ((width a) -1) && (fst r) < ((height a)-1) =	
			((abs(((getRed (getColumn (getRow a (fst r)) ((snd r)-1)) - 
			(getRed (getColumn (getRow a (fst r)) ((snd r)+1))))^2)) +
			(abs(((getGreen (getColumn (getRow a (fst r)) ((snd r)-1))) - 
			(getGreen (getColumn (getRow a (fst r)) ((snd r)+1))))^2)) +
			(abs(((getBlue (getColumn (getRow a (fst r)) ((snd r)-1))) - 
			(getBlue (getColumn (getRow a (fst r)) ((snd r)+1))))^2))) +
			(abs(((getRed (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getRed (getColumn (getRow a ((fst r)+1)) (snd r))))^2)) +
			(abs(((getGreen (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getGreen (getColumn (getRow a ((fst r)+1)) (snd r))))^2)) +
			(abs(((getBlue (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getBlue (getColumn (getRow a ((fst r)+1)) (snd r))))^2))))))
	| (fst r) < 1 && (snd r) > 0 =
			((abs(((getRed (getColumn (getRow a (fst r)) ((snd r)-1)) - 
			(getRed (getColumn (getRow a (fst r)) ((snd r)+1))))^2)) +
			(abs(((getGreen (getColumn (getRow a (fst r)) ((snd r)-1))) - 
			(getGreen (getColumn (getRow a (fst r)) ((snd r)+1))))^2)) +
			(abs(((getBlue (getColumn (getRow a (fst r)) ((snd r)-1))) - 
			(getBlue (getColumn (getRow a (fst r)) ((snd r)+1))))^2))) +
			(abs(((getRed (getColumn (getRow a ((height a)-1)) (snd r)) - 
			(getRed (getColumn (getRow a ((fst r)+1)) (snd r))))^2)) +
			(abs(((getGreen (getColumn (getRow a ((height a)-1)) (snd r)) - 
			(getGreen (getColumn (getRow a ((fst r)+1)) (snd r))))^2)) +
			(abs(((getBlue (getColumn (getRow a ((height a)-1)) (snd r)) - 
			(getBlue (getColumn (getRow a ((fst r)+1)) (snd r))))^2))))))
	| (fst r) > 0 && (snd r) < 1 = 
			((abs(((getRed (getColumn (getRow a (fst r)) ((width a)-1)) - 
			(getRed (getColumn (getRow a (fst r)) ((snd r)+1))))^2)) +
			(abs(((getGreen (getColumn (getRow a (fst r)) ((width a)-1))) - 
			(getGreen (getColumn (getRow a (fst r)) ((snd r)+1))))^2)) +
			(abs(((getBlue (getColumn (getRow a (fst r)) ((width a)-1))) - 
			(getBlue (getColumn (getRow a (fst r)) ((snd r)+1))))^2))) +
			(abs(((getRed (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getRed (getColumn (getRow a ((fst r)+1)) (snd r))))^2)) +
			(abs(((getGreen (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getGreen (getColumn (getRow a ((fst r)+1)) (snd r))))^2)) +
			(abs(((getBlue (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getBlue (getColumn (getRow a ((fst r)+1)) (snd r))))^2))))))
	| (fst r) > 0 && (snd r) >= ((width a) -1) =
			((abs(((getRed (getColumn (getRow a (fst r)) ((snd r)-1)) - 
			(getRed (getColumn (getRow a (fst r)) 0)))^2)) +
			(abs(((getGreen (getColumn (getRow a (fst r)) ((snd r)-1))) - 
			(getGreen (getColumn (getRow a (fst r)) 0 )))^2)) +
			(abs(((getBlue (getColumn (getRow a (fst r)) ((snd r)-1))) - 
			(getBlue (getColumn (getRow a (fst r)) 0 )))^2))) +
			(abs(((getRed (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getRed (getColumn (getRow a ((fst r)+1)) (snd r))))^2)) +
			(abs(((getGreen (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getGreen (getColumn (getRow a ((fst r)+1)) (snd r))))^2)) +
			(abs(((getBlue (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getBlue (getColumn (getRow a ((fst r)+1)) (snd r))))^2))))))
	| (fst r) >= ((height a)-1) && (snd r) > 0 =
			((abs(((getRed (getColumn (getRow a (fst r)) ((snd r)-1)) - 
			(getRed (getColumn (getRow a (fst r)) ((snd r)+1))))^2)) +
			(abs(((getGreen (getColumn (getRow a (fst r)) ((snd r)-1))) - 
			(getGreen (getColumn (getRow a (fst r)) ((snd r)+1))))^2)) +
			(abs(((getBlue (getColumn (getRow a (fst r)) ((snd r)-1))) - 
			(getBlue (getColumn (getRow a (fst r)) ((snd r)+1))))^2))) +
			(abs(((getRed (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getRed (getColumn (getRow a 0) (snd r))))^2)) +
			(abs(((getGreen (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getGreen (getColumn (getRow a 0) (snd r))))^2)) +
			(abs(((getBlue (getColumn (getRow a ((fst r)-1)) (snd r)) - 
			(getBlue (getColumn (getRow a 0) (snd r))))^2))))))
	| otherwise = 0


getColumn :: [RGB] -> Int -> RGB
getColumn a b = a !! b 

getRow :: Grid RGB -> Int -> [RGB]
getRow (G a) b = a !! b

--Energy Helpers
getRed :: RGB -> Int
getRed (RGB a b c) = a

getGreen :: RGB -> Int
getGreen (RGB a b c) = b

getBlue :: RGB -> Int
getBlue (RGB a b c) = c
--Energy Helpers


--Given a grid of RGBs, create and return a grid storing the energy at each point in the argument grid.
energies :: Grid RGB -> Grid NodeEnergy
energies (G a) = G (combine (G a))

--Prints out a 2D array of energies
---------------------------------------------------------------
combine :: Grid RGB -> [[NodeEnergy]]
combine (G a) = makeTwo (G a) (myIterate (G a))

myIterate :: Grid RGB -> [NodeEnergy]
myIterate (G a) =   [ps| 
	ps  <-  [energyAt (G a) xs | 
	xs <- [(x,y)| 
	x <- [0..(height (G a))-1], 
	y <- [0..(width (G a))-1]]]]

makeTwo :: Grid a -> [b] -> [[b]]
makeTwo (G a) [] =  []
makeTwo (G a) b = take (width (G a)) b:[] ++ makeTwo (G a) (drop (width (G a)) b)
---------------------------------------------------------------

--Counts the total number of Nodes in the grid (sub 1 for range)
totalCount :: Grid a -> Int
totalCount (G a) = (height (G a)) * (width (G a))

-- Create a Grid Packet
gridPacket :: Grid RGB -> Grid Packet
gridPacket (G a) = G (makeTwo (G a) (packetList (G a)))


packetList :: Grid RGB -> [Packet]
packetList (G a) = 
	let 	x = createCoords (G a)
		y = getRGB (G a)
		z = myIterate (G a)
	in	[(x!!c, y!!c, z!!c) | c <- [0..(totalCount (G a) -1)]] 

--Provides an array of Coords
createCoords :: Grid RGB -> [Coord]
createCoords (G a) = [(x,y) | x <- [0..(height (G a)) -1], y <-[0..(width (G a))-1]]  

--Provides an array of RGB's
getRGB :: Grid RGB -> [RGB]
getRGB (G a) = [getColumn (getRow (G a) (fst r)) (snd r) | r <- createCoords (G a)]

------------------------------------------------------------------
--Will be used to construct the (Node,Node,Node) part of the Node datatype
nextGroups :: [a] -> a -> [(a,a,a)]
nextGroups a b = firstGroups a b ++ lastGroup a b

--Builds all but the last group for the 'nextGroups' function
firstGroups :: [a] -> a -> [(a,a,a)]
firstGroups []  _ = [] 
firstGroups (x:y:xs) g = (g,x,y):(if length xs<1 then firstGroups [] x else firstGroups (y:xs) x)

--Builds the last group for the 'nextGroups' function
lastGroup :: [a] -> a -> [(a,a,a)]
lastGroup a b = [(getSecond((firstGroups a b)!!((length a)-2)), 
		getThird((firstGroups a b)!!((length a)-2)), 
		getFirst((firstGroups a b)!!0))]

--returns the first element of a 3 element tuple
getFirst :: (a,a,a) -> a
getFirst (a,_,_) = a

--returns the second element of a 3 element tuple
getSecond :: (a,a,a) -> a
getSecond (_,a,_) = a

--returns the  third element of a 3 element tuple
getThird :: (a,a,a) -> a
getThird (_,_,a) = a
-------------------------------------------------------------------


buildNodeFromPacket :: Packet -> (Node,Node,Node) -> Node
buildNodeFromPacket = undefined

buildRowFromPackets = undefined
packetsToNodes = undefined
findVerticalPath = undefined
findHorizontalPath = undefined
removeVerticalPath = undefined
removeHorizontalPath = undefined
gridToFile = undefined
fileToGrid = undefined
