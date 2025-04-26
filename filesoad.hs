import Control.Monad

(|>) :: a -> (a -> b) -> b
x |> f = f x

(||>) :: Maybe a -> (a -> Maybe b) -> Maybe b
x ||> f = x >>= f

infixl 1 |>

infixl 1 ||>

type Name = String

type Data = String

data FSItem = Folder Name [FSItem] | File Name Data deriving (Show)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

modifyName :: (Name -> Name) -> FSZipper -> FSZipper
modifyName f (File o d, c) = (File (f o) d, c)
modifyName f (Folder o d, c) = (Folder (f o) d, c)

fsUp :: FSZipper -> Maybe FSZipper
fsUp (i, FSCrumb n ls rs : c) = Just (Folder n (ls ++ [i] ++ rs), c)

fsTo :: Name -> FSZipper -> Maybe FSZipper
-- fsTo n (Folder fn its, c) =
--   case break (nameIn n) its of
--     (ls, i : rs) -> Just (i, FSCrumb fn ls rs : c)
--     _ -> Nothing
fsTo n (Folder fn its, c)
  | (ls, i : rs) <- break (nameIn n) its = Just (i, FSCrumb fn ls rs : c)
  | otherwise = Nothing

fsto _ (File _ _, _) = Nothing

nameIn :: Name -> FSItem -> Bool
nameIn searched (Folder n _) = n == searched
nameIn searched (File n _) = n == searched

unwrap :: Maybe a -> a
unwrap (Just x) = x
unwrap Nothing = error "jump in the fire"

myDisk :: FSItem
myDisk =
  Folder
    "root"
    [ File "goat_yelling_like_man.wmv" "baaaaaa",
      File "pope_time.avi" "god bless",
      Folder
        "pics"
        [ File "ape_throwing_up.jpg" "bleargh",
          File "watermelon_smash.gif" "smash!!",
          File "skull_man(scary).bmp" "Yikes!"
        ],
      File "dijon_poupon.doc" "best mustard",
      Folder
        "programs"
        [ File "fartwizard.exe" "10gotofart",
          File "owl_bandit.dmg" "mov eax, h00t",
          File "not_a_virus.exe" "really not a virus",
          Folder
            "source code"
            [ File "best_hs_prog.hs" "main = print (fix error)",
              File "random.hs" "main = print 4"
            ]
        ]
    ]

main :: IO ()
main = do
  let fdisk = Just (myDisk, []) ||> fsTo "pics" ||> fsTo "watermelon_smash.gif" ||> fsUp ||> fsUp ||> fsTo "programs" ||> fsTo "not_a_virus.exe"
   in print $ fst $ unwrap fdisk
