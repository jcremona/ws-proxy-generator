
--run1 = f $ (runStReader $ asx [func, func2, func3, func4] []) [("print",typ)] []
--      where f Nothing = error "Nothing"
--            f (Just (s,_)) = map (\(n,t) -> n ++ " :: " ++ prettyPrinterType t) s


--run2 :: [String]--([(String, Type)], [(String, Type)])
--run2 = f $ (runStReader $ analyze [func5] []) [("length",typ2)] []
--      where f (Left e) = [show e]
--            f (Right (s,_)) = map (\(n,t) -> n ++ " :: " ++ prettyPrinterType t) s



--run func dts = f $ (runStReader $ asx func dts) [("invokeWS",callWSType)] []
--      where f Nothing = error "Nothing"
--            f (Just (s,_)) = map (\(n,t) -> n ++ " :: " ++ prettyPrinterType t) s

--runL f d = run [f] [d]


--func = DefFun "sum" [("a",  $ intType), ("b",  $ intType), ("c",  $ stringType)] (Call "show" [Free "a", Free "b"])
--func2 = DefFun "show" [("a",  $ intType), ("b",  $ intType)] (Call "print" [Free "a", Free "b"])
--func3 = DefFun "exc" [("a",  $ intType), ("b",  $ stringType)] (Call "sum" [Free "a", Free "a"])
--func4 = DefFun "apply" [("sum", functionType ( stringType) ( intType)), ("b",  $ stringType)] (Call "sum" [Free "b"])
--typ = functionType ( intType) (functionType ( intType) ( stringType))

-- TODO para este ejemplo eq (Empty) (T ty) debe dar false, pero para el de abajo debe dar True! hay que distinguir entre la lista vacia y un tipo de Lista Parametrico 
--func5 = DefFun "len" [("a", TList EmptyListType)] (Call "length" [Free "a"]) (functionType (TList EmptyListType) ( intType))
--typ2 = functionType (TList $ T $  intType) ( intType)

--func5 = DefFun "len" [] (Call "length" []) (functionType (TList EmptyListType) ( intType))
--typ2 = functionType (listType intType) ( intType)


--func5 = DefFun "len" [("xs", stringType)] (listExpr []) (functionType stringType $ listType $ listType intType)


--len :: String -> [[Int]]
--len s = [[]]
--typ3 = functionType ( stringType) (functionType ( stringType) ( stringType))

--len :: [a] -> Int
--len a = lt a

--lt :: [Int] -> Int
--lt = const 2
