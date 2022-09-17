import DataFile

createEmptyFreqList :: [a] -> [(a, [b])]

createEmptyFreqList [] =[]
createEmptyFreqList (x:xs)=(x,[]):(createEmptyFreqList xs)

emptyAllUsersStats []=[]
emptyAllUsersStats (x:xs)=(x, (createEmptyFreqList items)): emptyAllUsersStats xs

getUserFromPurchaseHistory user []= error "user not found"
getUserFromPurchaseHistory user ((u,s):xs)
	| user==u=s
	|otherwise= getUserFromPurchaseHistory user xs

getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]

getAllUsersStats []=[]
getAllUsersStats historyPurchases= getAllUsersStatsHelper (emptyAllUsersStats users )

getAllUsersStatsHelper []=[]
getAllUsersStatsHelper ((user,list):xs)= (user,(fillInList user list)):(getAllUsersStatsHelper xs)

fillInList user []=[]
fillInList user ((item,[]):xs)= (item,(fillInInnerList user item items)):(fillInList user xs)

fillInInnerList user givenItem []=[]
fillInInnerList user givenItem (item:xs)
	|givenItem==item=fillInInnerList user givenItem xs
	|(isZero user givenItem item)==True=fillInInnerList user givenItem xs
	|otherwise=(item,(howMany givenItem item (getUserFromPurchaseHistory user purchasesHistory))):(fillInInnerList user givenItem xs)

howMany item1 item2 []=0
howMany item1 item2 (x:xs)
	|(elem item1 x)==True && (elem item2 x)==True= 1 + (howMany item1 item2 xs)
	|otherwise=howMany item1 item2 xs

isZero user givenItem item=
	if howMany givenItem item (getUserFromPurchaseHistory user purchasesHistory)==0
	then True
	else False

freqListItems:: String -> [(String, Int)] 

freqListItems user= freqListItemsHelper (getUserFromUserStats user (getAllUsersStats purchasesHistory)) items

freqListItemsHelper userStats []=[]
freqListItemsHelper userStats (x:xs)
	|findFrequency x userStats==0=[] ++ freqListItemsHelper userStats xs
	|otherwise=[(x, (findFrequency x userStats) )] ++ (freqListItemsHelper userStats xs)

findFrequency x []=0
findFrequency x ((item,list):xs)= findFrequencyHelper x list + findFrequency x xs

findFrequencyHelper x []= 0
findFrequencyHelper x ((item,number):xs)
	|x==item=number
	|otherwise=findFrequencyHelper x xs

getUserFromUserStats user []= error "user not found"
getUserFromUserStats user ((u, list):xs)
	| user==u=list
	|otherwise= getUserFromUserStats user xs

freqListCart:: String ->[String] -> [(String, Int)]
freqListCart user cart=freqListCartHelper (getFreqListFromUserStats cart (getUserFromUserStats user (getAllUsersStats purchasesHistory))) items

freqListCartHelper freqList []=[]
freqListCartHelper freqList (x:xs)
	|findFrequency x freqList==0=[] ++ freqListCartHelper freqList xs
	|otherwise=[(x, (findFrequency x freqList) )] ++ (freqListItemsHelper freqList xs)

getFreqListFromUserStats [] userstats=[]
getFreqListFromUserStats (x:xs) userstats= getFreqListFromUserStatsHelper x userstats ++ getFreqListFromUserStats xs userstats

getFreqListFromUserStatsHelper x []=[]
getFreqListFromUserStatsHelper x ((item,list):xs)
	|x==item=[(item,list)]
	|otherwise= getFreqListFromUserStatsHelper x xs

freqListCartAndItems:: String -> [String] -> [(String, Int)]
freqListCartAndItems user cart= combineFrequencyLists (freqListItems user) (freqListCart user cart)

combineFrequencyLists x []=x
combineFrequencyLists ((item1,freq1):xs)((item2,freq2):ys)=
	if item1==item2
	then (item1,(freq1+freq2)):(combineFrequencyLists xs ys)
	else (item1,freq1):(combineFrequencyLists xs ((item2,freq2):ys))

purchasesIntersection :: Eq a => [ ( a , [ ( a , Int ) ] ) ] -> [ ( a , [ ( a , [ ( a , Int ) ] )] ) ] -> [ [ ( a , [ ( a , Int ) ] ) ] ]

purchasesIntersection list1 []=[]
purchasesIntersection list1 ((user,list2):ys)= (purchasesIntersectionHelper list1 list2) :(purchasesIntersection list1 ys)

purchasesIntersectionHelper [][]=[]
purchasesIntersectionHelper ((item1,list1):xs) ((item2,list2):ys)
	|list1==[]=(purchasesIntersectionHelper xs ys)
	|list2==[]=(purchasesIntersectionHelper xs ys)
	|otherwise=(item1,((intersection list1 list2) ++ (difference list1 list2) ++ (difference list2 list1))):(purchasesIntersectionHelper xs ys)

intersection [] list=[]
intersection ((item,freq):xs) list= (intersectionHelper (item,freq) list )++(intersection xs list)

intersectionHelper (item1,freq1) []=[]
intersectionHelper (item1,freq1) ((item2,freq2):xs)
	|item1==item2= [(item1,(freq1 + freq2))]
	|otherwise=intersectionHelper (item1,freq1) xs


difference [] list=[]
difference ((item,freq):xs) list= (differenceHelper (item,freq) list )++(difference xs list)
differenceHelper (item1,freq1) []= [(item1,freq1)]
differenceHelper (item1,freq1) ((item2,freq2):xs)
	|item1==item2= []
	|otherwise=differenceHelper (item1,freq1) xs

freqListUsers :: String -> [(String, Int)]
freqListUsers user= freqListUsersHelper (purchasesIntersection (getUserFromUserStats user (getAllUsersStats purchasesHistory)) (getAllExcept user)) items

freqListUsersHelper freqList []=[]
freqListUsersHelper freqList (x:xs)
	|helper2 x freqList==0=[] ++ freqListUsersHelper freqList xs
	|otherwise=[(x, (helper2 x freqList) )] ++ (freqListUsersHelper freqList xs)

helper2 item []=0
helper2 item (x:xs)= (findFrequency item x) + (helper2 item xs)

getIndex user num []= error "user not found"
getIndex user num (x:xs)
	|x==user=num
	|otherwise= getIndex user (num + 1) xs

getAllExcept user= getAllExceptHelper 0 (getIndex user 0 users)

getAllExceptHelper i n
	|i==length (getAllUsersStats purchasesHistory)=[]
	|i==n=getAllExceptHelper (i+1) n
	|otherwise= [ (getAllUsersStats purchasesHistory)!!i] ++ (getAllExceptHelper (i+1) n)

recommendEmptyCart :: String -> String

recommendEmptyCart  user
	|length (getProbability (freqListItems user))==0=[]
	|otherwise=(getProbability (freqListItems user))!!(randomZeroToX (length (getProbability (freqListItems user))-1))

getProbability []=[]
getProbability ((item,num):xs)= (insertNTimes 0 num item) ++ getProbability xs

insertNTimes i n x
	|i==n=[]
	|otherwise=x:insertNTimes (i+1) n x

recommendBasedOnItemsInCart :: String -> [String] -> String

recommendBasedOnItemsInCart  user cart
	|length (getProbability (freqListCartAndItems user cart))==0=[]
	|otherwise=(getProbability (freqListCartAndItems user cart))!!(randomZeroToX (length (getProbability(freqListCartAndItems user cart))-1))

recommendBasedOnUsers :: String -> String

recommendBasedOnUsers user
	|length (getProbability (freqListUsers user))==0=[]
	|otherwise=(getProbability (freqListUsers user))!!(randomZeroToX (length (getProbability (freqListUsers user))-1))

recommend :: String -> [String] -> String
recommend user cart
	|recommendBasedOnItemsInCart user cart==[] && recommendBasedOnUsers user==[]=items!!(randomZeroToX (length (items)-1))
	|recommendBasedOnItemsInCart user cart==[]= recommendBasedOnUsers user
	|recommendBasedOnUsers user==[]=recommendBasedOnItemsInCart user cart
	|otherwise=[(recommendEmptyCart user),(recommendBasedOnItemsInCart user cart)]!!(randomZeroToX (1))
	
