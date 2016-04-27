import Data.Maybe
import Prelude
import System.IO
import Data.Char
import Data.String
import Data.List
import System.Exit

getAndIndexs s = fromJust(findIndex (=='&') s)

getAndIndex s = findIndex (=='&') s

getArrowIndexs s = fromJust(findIndex (=='-') s)

getArrowIndex s = findIndex (=='-') s

getCommaIndexs s = fromJust(findIndex (==',') s)

getCommaIndex s = findIndex (==',') s

txtFltr s= filter (/=' ') (filter (/='\t') s)

splitStr index s = splitAt index s


splitComma strng = if(null strng)
				 then []
				 else do
					 if(elem ',' strng)
					 then do
					 	let index = getCommaIndex strng
					 	if(isNothing index)
					 	--then strng:[]
					 	then []
					 	else do
						 	let spStr = splitStr (fromJust(index)) strng
						 	fst(spStr) : splitComma (tail(snd spStr))
					 else strng : [] 


isLetOrDigit :: String -> Bool
isLetOrDigit [] = True
isLetOrDigit (x:xs) 
	| ((x == ' ')|| isSpace x) = False  --NA
	| ((isDigit x == True  || isLetter x == True) && isLetOrDigit xs) == True = True
	| otherwise = False


typeVarName :: String -> Bool
typeVarName [] = True --NAA
typeVarName (x:xs)
	| ((x == ' ')||isSpace x) = False  --NA
	| isLetter x == True = isLetOrDigit xs
	| otherwise = False
	

tpeVar :: String -> Bool
tpeVar [] = False
tpeVar (x:xs)
	| x == '`' = typeVarName xs
	| otherwise = False

--NA
checkPrimTypeSpace :: String -> Bool
checkPrimTypeSpace [] = True
checkPrimTypeSpace (x:xs)
	| ((x == ' ')||isSpace x) = False
	| otherwise = checkPrimTypeSpace xs


primType strng = if(strng == "int" || strng == "real" || strng == "str")
				then True
				else False


checkArgListTypee :: [String] -> Bool
checkArgListTypee [] = True
checkArgListTypee (x:xs) = typee x && checkArgListTypee xs

checkArgList :: String -> Bool
checkArgList [] = True
checkArgList strng = do
					let spStr = splitComma strng
					checkArgListTypee spStr


funcType1 :: String -> Bool
funcType1 strng = if (head strng == '(' && last strng == ')')
				then checkArgList(tail(init strng))     --removing ()
				else False


lstTpe :: String -> Bool
lstTpe strng = if (head strng == '[' && last strng == ']')
				then do
						if(null(tail(init strng)))
						then False
						else typee(tail(init strng))
				else False

funcType :: String -> Bool
funcType strng = do
	let txt = txtFltr strng 
	--let spStr = splitArrow txt 
	if (elem '-' strng)
	then do
		let index = getArrowIndex txt
		if(isNothing index)
		then False
		else do
			let spStr = splitStr (fromJust(index)) txt 
			funcType1 (fst spStr) && typee(tail(tail(snd spStr)))
	else False


typee :: String -> Bool
typee strng
	| (tpeVar strng) = True
	| (primType strng) = True
	| (lstTpe strng) = True
	| (funcType strng) = True
	| otherwise = False

unifyPrimType :: String -> String -> IO()
unifyPrimType str1 str2 = if(str1=="int" && str2=="int")
						  then do
						  		putStrLn "int"
						  		takeInput True
						  else if(str1=="real" && str2=="real")
						  		then do
						  				putStrLn "real"
						  				takeInput True
						  		else if(str1=="str" && str2=="str")
						  			 then do
						  			 		putStrLn "str"
						  			 		takeInput True
						  			 else do
						  			 		putStrLn "BOTTOM"
						  			 		takeInput False

compareListAndString :: String->[String]->Bool
compareListAndString s (x:xs) = if(s==x)
								then True
								else if(xs /= [])
										then compareListAndString s xs
										else False

getListFuncString :: String -> [String]  -- (int,int)->`a ==> ["`a","int","int"]
getListFuncString funcStr = do
								--let txt = txtFilter funcStr
								let index = getArrowIndex funcStr
								let spStr = splitStr (fromJust(index)) funcStr
								tail(tail(snd spStr)) : splitComma(tail(init(fst spStr)))

getArgListStr :: String -> [String] -- (int,int)->`a ==> ["int","int"]
getArgListStr funcStr = do
							let index = getArrowIndex funcStr
							let spStr = splitStr (fromJust(index)) funcStr
							splitComma(tail(init(fst spStr)))

--NA:start

unifyFuncAndPrimType2 :: String -> [String] -> [String]
unifyFuncAndPrimType2 _ [] = []
unifyFuncAndPrimType2 primStr (x:xs) = unifyfunc x primStr :  unifyFuncAndPrimType2 primStr xs


unifyFuncAndPrimType :: String-> String -> IO()
unifyFuncAndPrimType funcStr primStr = do
										let funcStrList = getListFuncString funcStr
										--let argList = splitComma(tail(init funcStr))  -- int,int=> ["int","real","int"]   real
										if(length funcStrList == 0)
										then do
												putStrLn primStr
												takeInput True
										else do
											if(compareListAndString primStr funcStrList)
											then do
													putStrLn "BOTTOM"
													takeInput False
											else do
													--putStrLn "argList and primType are not matching"
													let unifyFt_Pt = unifyFuncAndPrimType2 primStr funcStrList
													if(compareListAndString "BOTTOM" unifyFt_Pt)
													then do
															putStrLn "BOTTOM"
															takeInput False
													else do
															putStrLn primStr
															takeInput True


unifyFuncAndVarType :: String-> String -> IO()
unifyFuncAndVarType funcStr primStr = do
										let funcStrList = getListFuncString funcStr
										if(compareListAndString primStr funcStrList)
										then do
												putStrLn "BOTTOM"
												takeInput False
										else do
												putStrLn primStr
												takeInput True

unifyFuncAndList :: String -> String -> IO()
unifyFuncAndList funcStr listStr = do									
									let argStrList = getArgListStr funcStr	
									let funcStrList = getListFuncString funcStr								
									if(null argStrList)
									then do
											putStrLn listStr
											takeInput True
									else do
											let unifyFt_Lt = unifyFuncAndPrimType2 (tail(init listStr)) funcStrList	
											print unifyFt_Lt			
											if(compareListAndString "BOTTOM" unifyFt_Lt)
											then do
													putStrLn "BOTTOM"
													takeInput False
											else do
													putStrLn listStr
													takeInput True


reverseFuncList :: [String] -> [String]
reverseFuncList lst1 = do
						let s1 = take 1 lst1
						let list1 = reverse (drop 1 lst1)
						s1 ++ list1


unifyfunc :: String-> String -> String
unifyfunc str1 str2   
	| (primType str1 && primType str2) -- primitive_type && primitive_type
		= 	if(str1 == str2)  -- check for int==int || real==real || str==str
			then str1
			else "BOTTOM"   -- int == real || real ==str  || etc..
	| (primType str1 && tpeVar str2) --primitive_type && var_tpe
		= str1
	| (tpeVar str1 && primType str2) -- var_tpe && primitive_type
		= str2
	| (tpeVar str1 && tpeVar str2) -- var_tpe && var_tpe
		= str2
	| (lstTpe str1) -- str1 => list_type
		= do
			let ltype_str = tail(init str1)
			if(tpeVar ltype_str && primType str2) -- primitive_type && [type_var]
			then "BOTTOM"
			else do
				let uniFuncStr = unifyfunc ltype_str str2
				if(uniFuncStr == "BOTTOM")
				then uniFuncStr
				else "["++ uniFuncStr ++ "]"
	| (lstTpe str2) -- str2 => list_type
		= do
			let ltype_str = tail(init str2)
			if(tpeVar ltype_str && primType str1)  -- [type_var] && primitive_type
			then "BOTTOM"
			else do
				let uniFuncStr = unifyfunc str1 ltype_str
				if(uniFuncStr == "BOTTOM")
				then uniFuncStr
				else "["++ uniFuncStr ++ "]"
	| otherwise = ""


unifyFuncFunc :: [String] -> [String] -> [String]
unifyFuncFunc [] [] = []
unifyFuncFunc (x:xs) (y:ys) =  unifyfunc x y : unifyFuncFunc xs ys

--NAA:start
makeKeyValuePair :: [String] -> [String] -> [(String,String)]
makeKeyValuePair [] [] = []
makeKeyValuePair (x:xs) (y:ys) = makeKeyValuePair1 x y : makeKeyValuePair xs ys

makeKeyValuePair1 :: String -> String -> (String, String)
makeKeyValuePair1 str1 str2 
	| (tpeVar str1 && tpeVar str2) -- var_tpe && var_tpe
		= (str1,str2)
	| (tpeVar str1 && primType str2) -- var_tpe && prim_type
		= (str1,str2)
	| (tpeVar str2 && primType str1) -- prim_type && var_tpe
		= (str2, str1)
	| (tpeVar str1 && lstTpe str2) -- var_tpe && listType
		= do
			let ltype_str = tail(init str2)
			if(tpeVar ltype_str)
			then (str1,ltype_str)
			else if(primType ltype_str)
					then (str1, ltype_str)
					else ("","") 
	| (tpeVar str2 && lstTpe str1) -- listType && var_tpe
		= do
			let ltype_str = tail(init str1)
			if(tpeVar ltype_str)
			then (ltype_str,str2)
			else if(primType ltype_str)
					then (str2,ltype_str)
					else ("","")	
	|otherwise = ("","")

--NAA:end

unifyListAndVar :: String -> String -> IO()
unifyListAndVar listStr varStr = 	if(funcType(tail(init listStr)))  -- [func_type] && vartype
									then unifyFuncAndVarType (tail(init listStr)) varStr
									else if((unifyfunc (tail(init listStr)) varStr) == "BOTTOM")
											then do
													putStrLn "BOTTOM"
													takeInput False
											else do
													putStrLn listStr
													takeInput True


--NAA:start
checkKeyPair :: String -> [(String,String)] -> String
checkKeyPair str [] = str
checkKeyPair unifyStr (x:xs) = 	if(head unifyStr == '[' && last unifyStr == ']')
								then do										
										if(tail(init unifyStr) == fst x)
										then "["++snd x++"]"
										else checkKeyPair unifyStr xs
								else if(unifyStr == fst x)
										then snd x
										else checkKeyPair unifyStr xs


computeUnifyAndKeyPair :: [String] -> [(String,String)] -> [String]
computeUnifyAndKeyPair [] _ = []
computeUnifyAndKeyPair (x:xs) keyPairList = checkKeyPair x keyPairList : computeUnifyAndKeyPair xs keyPairList

--NAA:end

--NAAA:start
checkForKey2 :: (String,String) -> (String,String) -> Bool
checkForKey2 keyVal1 keyVal2 = 	if(keyVal1 /= keyVal2)
								then do
										if(fst keyVal1 == fst keyVal2)
										then do
												if(snd keyVal1 == snd keyVal2)  --("`d","int"),("`d","int")
												then True
												else False
										else True
								else True

unifyFuncAndPrimTypeStr :: String-> String -> String
unifyFuncAndPrimTypeStr funcStr primStr = do
											let funcStrList = getListFuncString funcStr
											--print(funcStrList)
											--let argList = splitComma(tail(init funcStr))  -- int,int=> ["int","real","int"]   real
											if(length funcStrList == 0)
											then primStr
											else do
												if(compareListAndString primStr funcStrList)
												then "BOTTOM"
												else do
														let unifyFt_Pt = unifyFuncAndPrimType2 primStr funcStrList
														if(compareListAndString "BOTTOM" unifyFt_Pt)
														then "BOTTOM"
														else primStr


unifyListList:: String-> String -> String
-- pass raw list as input i.e [`a] [`b]
unifyListList list1 list2 =	do
							let list1Str = tail(init list1)
							let list2Str = tail(init list2)
							if(lstTpe list1Str && lstTpe list2Str)   --list_type & list_type
							then "["++(unifyListList list1Str list2Str)++"]"
							else do
								if(lstTpe list1Str)
								 then do
										let listStr = tail(init list1Str)
										if(lstTpe listStr)
										then "["++(unifyListList listStr list2Str)++"]"
										else if(tpeVar listStr && primType list2Str)  -- [type_var] && primitive_type
												then "BOTTOM"
												else if(primType listStr && primType list2Str) -- [primitive_type] && primitive_type
														then do
																if(listStr == list2Str)
																then "["++list2Str++"]"
																else "BOTTOM"
														else if(tpeVar listStr && tpeVar list2Str)  -- [type_var] && type_var
																then do "["++list1Str++"]"
																else if(primType listStr && tpeVar list2Str) --[primitive_type] && type_var
																		then "["++listStr++"]"
																		else if(primType listStr && funcType list2Str) -- [primitive_type] && func_type
																				then "BOTTOM"
																				else if(funcType listStr && tpeVar list2Str) --[func_type] && type_var
																						then do
																								let funcStrList = getListFuncString listStr
																								if(compareListAndString list2Str funcStrList)
																								then "BOTTOM"
																								else "["++list2Str++"]"
																						else "BOTTOM"
								 else if(lstTpe list2Str)
										then do
												let listStr = tail(init list2Str)
												if(lstTpe listStr)
												then "["++(unifyListList listStr list1Str)++"]"
												else if(tpeVar listStr && primType list1Str)  -- [type_var] && primitive_type
														then "BOTTOM"
														else if(primType listStr && primType list1Str) -- [primitive_type] && primitive_type
																then do
																		if(listStr == list1Str)
																		then "["++listStr++"]"
																		else "BOTTOM"
																else if(tpeVar listStr && tpeVar list1Str)  -- [type_var] && type_var
																		then "["++list2Str++"]"
																		else if(primType listStr && tpeVar list1Str) --[primitive_type] && type_var
																				then "["++listStr++"]"
																				else if(primType listStr && funcType list1Str) -- [primitive_type] && func_type
																						then "BOTTOM"
																						else if(funcType listStr && tpeVar list1Str) --[func_type] && type_var
																								then do
																										let funcStrList = getListFuncString listStr
																										if(compareListAndString list1Str funcStrList)
																										then "BOTTOM"
																										else "["++list1Str++"]"
																								else "BOTTOM"
										else if(tpeVar list1Str && primType list2Str)  -- [type_var] && [primitive_type]
												then "["++list2Str++"]"
												else if(primType list1Str && primType list2Str) -- [primitive_type] && [primitive_type]
														then do
																if(list1Str == list2Str)
																then "["++list2Str++"]"
																else "BOTTOM"
														else if(tpeVar list1Str && tpeVar list2Str)  -- [type_var] && [type_var]
																then "["++list2Str++"]"
																else if(primType list1Str && tpeVar list2Str) --[primitive_type] && [type_var]
																		then "["++list1Str++"]"
																		else if(funcType list1Str && primType list2Str) -- [func_type] && [primitive_type]
																				then "["++(unifyFuncAndPrimTypeStr list1Str list2Str)++"]"
																				else if(funcType list2Str && primType list1Str) -- [primitive_type] && [func_type]
																						then "["++(unifyFuncAndPrimTypeStr list2Str list1Str)++"]"
																						else if(funcType list1Str && tpeVar list2Str) --[func_type] && [type_var]
																								then do
																										let funcStrList = getListFuncString list1Str
																										if(compareListAndString list2Str funcStrList)
																										then "BOTTOM"
																										else "["++list2Str++"]"
																								else if(funcType list2Str && tpeVar list1Str) -- [type_var] && [func_type]
																										then do
																												let funcStrList = getListFuncString list2Str
																												if(compareListAndString list1Str funcStrList)
																												then "BOTTOM"
																												else "["++list1Str++"]"
																										else if(funcType list1Str && funcType list2Str) -- [func_type] && [func_type]
																												then do
																														let func1List = getListFuncString list1Str
																														let func2List = getListFuncString list2Str
																														if(length(func1List) == length(func2List))   --NAA: start
																														then do
																															 --putStrLn "num of elements are same"
																															 let unify = unifyFuncFunc (reverseFuncList func1List) (reverseFuncList func2List)																				 				  										 
																															 let revUnify = reverse unify
																															 if(compareListAndString "BOTTOM" unify)
																															 then "BOTTOM"
																															 else do
																															 	let keyValue = makeKeyValuePair (reverseFuncList func1List) (reverseFuncList func2List)
																															 	let finalList = computeUnifyAndKeyPair revUnify keyValue
																															 	if(checkForKey keyValue keyValue)
																															 	then "["++(displayFunctionFromFinalList1 finalList)++"]"
																															 	else "BOTTOM"
																														else "BOTTOM"
																												else "BOTTOM"


checkBottomInListUnify :: String->Bool
--return true if BOTTOM is found in list of []
checkBottomInListUnify [] = False
checkBottomInListUnify str = 	if(head str == '[' && last str == ']')
								then checkBottomInListUnify (tail(init str))
								else if(str == "BOTTOM")
										then True
										else False


checkForKey1 :: (String,String) -> [(String,String)] -> Bool
checkForKey1 _ [] = True
checkForKey1 keyVal (x:xs) = checkForKey2 keyVal x && checkForKey1 keyVal xs

checkForKey :: [(String,String)]-> [(String,String)] -> Bool
checkForKey [] _ = True
-- This function checks for duplicate key in keyValue tuple
checkForKey (x:xs) unifyList = checkForKey1 x unifyList && checkForKey xs unifyList

display2 _ [] = putStrLn ""
display2 len (x:xs) = do
						if(len > 2)
						then do
								putStr $ x++", "
								display2 (len-1) xs
						else if(len == 2)
								then do
										putStr $ x++") -> "
										display2 (len-1) xs
								else if(len == 1)
										then do
												putStr x
												display2 (len-1) xs
										else putStr ""


displayFunctionFromFinalList :: [String] -> IO()
displayFunctionFromFinalList funcList =	if(length funcList == 1)
										then putStrLn $ "() -> "++(head funcList)
										else do
												putStr "("
												display2 (length funcList) funcList

display3 _ [] = ""
display3 len (x:xs) = do
						if(len > 2)
						then x++", "++(display3 (len-1) xs)
						else if(len == 2)
								then x++") -> "++(display3 (len-1) xs)
								else if(len == 1)
										then x++(display3 (len-1) xs)
										else ""

displayFunctionFromFinalList1 :: [String] -> String
displayFunctionFromFinalList1 funcList = if(length funcList == 1)
											then "() -> "++(head funcList)
											else "("++(display3 (length funcList) funcList)
--NAAA: end

-- NA:end

startUnfictn str1 str2 
		| (primType str1 && primType str2)  -- primitive_type && primitive_type
			= unifyPrimType str1 str2
		| ((primType str1 && lstTpe str2) || (primType str2 && lstTpe str1))   -- primitive_type && list_type || list_type && primitive_type
	 		= do
 				putStrLn "BOTTOM"
 				takeInput False
	 	| ( primType str1 && tpeVar str2 ) -- primitive_type && typevar
	 		= do
				putStrLn str1
				takeInput True
	 	| (tpeVar str1 && primType str2) -- typevar && primitive_type
	 		= do
				putStrLn str2
				takeInput True
	 	| (tpeVar str1 && tpeVar str2)  -- typevar && typevar
	 		= do
	 			putStrLn str2
	 			takeInput True
		| (funcType str1 && primType str2)	--func_type && primitive_type
			= unifyFuncAndPrimType str1 str2
		| (funcType str2 && primType str1)  -- primitive_type && func_type
			= unifyFuncAndPrimType str2 str1
		| (funcType str1 && lstTpe str2) -- func_type && list_type
			= do
				putStrLn "BOTTOM"
				takeInput False
		| (funcType str2 && lstTpe str1) -- list_type && func_type
			= do
				putStrLn "BOTTOM"
				takeInput False
		| (lstTpe str1 && tpeVar str2) -- list_type && var_tpe
			= unifyListAndVar str1 str2
		| (lstTpe str2 && tpeVar str1) -- var_tpe && list_type
			= unifyListAndVar str2 str1
		| ((lstTpe str1 && lstTpe str2) || (lstTpe str2 && lstTpe str1))		 --list_type && list_type   --NAAA
			= do
				let unify = unifyListList str1 str2
				if(checkBottomInListUnify unify)
				then do
						putStrLn "BOTTOM"
						takeInput False
				else do
						putStrLn unify
						takeInput True
		| (funcType str1 && tpeVar str2) -- func_type && var_tpe
			= unifyFuncAndVarType str1 str2
		| (funcType str2 && tpeVar str1) -- var_tpe && func_type
			= unifyFuncAndVarType str2 str1
		| (funcType str1 && funcType str2) -- func_type && func_type
			= do
				let func1List = getListFuncString str1
				let func2List = getListFuncString str2
				if(length(func1List) == length(func2List))   --NAA: start
				then do
					 let unify = unifyFuncFunc (reverseFuncList func1List) (reverseFuncList func2List)																				 				  										 
					 let revUnify = reverse unify
					 if(compareListAndString "BOTTOM" unify)
					 then do																				 				  								
					 		putStrLn "BOTTOM"
					 		takeInput False
					 else do
					 	let keyValue = makeKeyValuePair (reverseFuncList func1List) (reverseFuncList func2List)
					 	let finalList = computeUnifyAndKeyPair revUnify keyValue
					 	if(checkForKey keyValue keyValue)
					 	then do
					 			displayFunctionFromFinalList finalList
					 			takeInput True
					 	else do
					 			putStrLn "BOTTOM"
					 			takeInput False
				else do
					 putStrLn "BOTTOM"
					 takeInput False
		| otherwise =  takeInput True
							 				  

takeInput:: Bool -> IO()
takeInput flag = if(not flag)
				 then exitSuccess
				 else do
						input <- getLine
						let txt = txtFltr input 
						if checkForSpace input
						then do
								if(txt /= "QUIT") 
								--if(n /= "QUIT")
								then do
										let index = getAndIndex txt 
										--let index = getAndIndex n
										if(isNothing index)
										then putStrLn "ERR"
										else do
											let spString = splitStr (fromJust(index)) txt				
											if(typee (fst spString) && typee(tail(snd spString)))
											then do
													--putStrLn "Unification logic"
													startUnfictn (fst spString) (tail(snd spString))
											else do
													putStrLn "ERR"
													takeInput False
								else takeInput False
						else do
								putStrLn "ERR"
								takeInput False


checkForIntT (x:xs) = if(x == 't')
						then checkForSpace xs 
						else False

checkForInt [] = True
checkForInt (x:xs) = if(x == 'n')
						then checkForIntT xs
						else False

checkForRealA [] = True
checkForRealA (x:xs) = if(x == 'a')
						then checkForRealL xs
						else False

checkForRealL [] = True
checkForRealL (x:xs) = if(x == 'l')
						then checkForSpace xs
						else False

checkForReal [] = True
checkForReal (x:xs) = if(x == 'e')
						then checkForRealA xs
						else False

checkForStrR [] = True
checkForStrR (x:xs) = if(x == 'r')
						then checkForSpace xs
						else False						

checkForStr [] = True
checkForStr (x:xs) = if(x == 't')
						then checkForStrR xs
						else False


checkForVarname [] = True
checkForVarname (x:xs) = if(x == ' ' || isSpace x)
							then False
							else checkForSpace xs


--flag indicate char is immidiate after `
checkForVarname1 _ [] = True
checkForVarname1 flag (x:xs) = 	if(flag)
								then do
										if(x == ' ')
										then False
										else checkForVarname1 False xs
								else do
									if(x == ',' || x == '&' || x == ' ')
									then checkForSpace xs
									else checkForVarname1 False xs


getListStr :: String -> String
getListStr [] = ""
getListStr (x:xs) = if(x /= ']')
					then x:getListStr xs
					else ""

	
{-
checkForSpace1 [] = True
checkForSpace1 (x:xs)
	| (x == '[') = checkForSpace1 xs
	| (x == '(') = checkForSpace1 xs
	| otherwise = checkSpaceInStr (concat x xs) (concat x xs)
-}

checkForSpace :: String -> Bool
checkForSpace [] = True
checkForSpace (x:xs)
	| ((x == ' ')||isSpace x) = checkForSpace xs
	| (x == 'i') = checkForInt xs
	| (x == 'r') = checkForReal xs
	| (x == 's') = checkForStr xs
	| (x == '`') = checkForVarname xs
	| otherwise = checkForSpace xs

--NAA:end

main :: IO()
main =do
		takeInput True