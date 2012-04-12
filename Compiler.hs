import System.Process
import System.Environment
import Control.Monad
import Data.List
import Data.Char

type Post = (String,String)

--------------------------------------------------
-- DATA ------------------------------------------
--------------------------------------------------

cgiSite = "https://www.cs.indiana.edu/cgi-pub/rrnewton/p423/compiler/compile.cgi"
authFile = ".p423-auth"          

----- The unchanging portion of the POST data sent to the site.
----- The ARGUMENT HANDLING section may be easily extended to allow for
----- command line options.
basePost :: [Post]
basePost = [ ("submit","Compile") , ("ioj","yes") , ("numpregs","2") , ("rvreg","rax")
           , ("rareg","r15") , ("fpreg","rbp") , ("apreg","rdx") , ("numaregs","2") ]

----- Old/New string pairs required to emulate MIME encoding
bodyFormatting = [ (' ',"+"), ('\'',"%27"), ('\n',"+")
                 , ('?',"%3F"), ('#',"%23")
                 , ('!',"%21") , ('"',"%22") , ('$',"%24")
                 , ('(',"%28") , (')',"%29") , ('*',"%2A")
                 , ('+',"%2B") , (',',"%2C") , ('-',"%2D")
                 , ('.',"%2E") , ('/',"%2F") , (':',"%3A")
                 , (';',"%3B") , ('<',"%3C") , ('=',"%3D")
                 , ('>',"%3E") , ('?',"%3F") , ('@',"%40")
                 , ('[',"%5B") , (']',"%5D") , ('^',"%5E")
                 , ('_',"%5F") , ('`',"%60") , ('~',"%7E") ]

postFormatting = [ ("&lt;","<") , ("&gt;",">") , ("[","(") , ("]",")") ]

--------------------------------------------------
-- FORMATTING ------------------------------------
--------------------------------------------------

----- Fortunately, the code output from the online compiler is returned
----- within a <pre> "preformatted" container, making it easy to extract
cleanOutput :: String -> String
cleanOutput = replaceAll postFormatting . unlines .
              drop 2 . init .
              reverse . dropWhile (not . isInfixOf "</pre>") .
              reverse . dropWhile (not . isInfixOf "<pre>") .
              lines

formatPost :: Post -> String
formatPost (k,v) = k ++ "=" ++ v

----- Takes code and other POST data, formatting it in k0=v0&k1=v1&... style
formatData :: String -> [Post] -> String
formatData body pd = intercalate "&" $
  map formatPost (("body", replaceChars bodyFormatting body):pd)

--------------------------------------------------
-- ARGUMENT HANDLING -----------------------------
--------------------------------------------------

----- Driver for parsing POST options. Other parsers are possible,
----- for instance, matching flags for curl arguments, rather than POST data
----- NB: This strategy ignores all bad input. Important input (like week number)
----- should be handled nicely with an error message rather than being ignored.
parsePostArgs :: String -> [String] -> [Post]
parsePostArgs w args = ("week",w):(getStart args ++ getPasses args ++ basePost)

----- Takes flag to match on command line, a default POST key/value pair
----- a continuation-like function (which is invoked upon the string
----- which follows the flag, should it occur), and the list of arguments
matchFlag :: String -> Post -> (String -> [Post]) -> [String] -> [Post]
matchFlag flag p@(key,def) f args =
  if' (null v) [p] (f (v !! 1))
    where v = dropWhile (not . isPrefixOf flag) args

----- Flag to match is "-s", default POST option is "startingpass=verify-scheme"
----- produces a single POST option "startingpass=s" for whatever s is given
getStart = matchFlag "-s" ("startingpass","verify-scheme") $
           (\s -> [("startingpass",s)])

----- Flag to match is "-p", default option is "allpasses=yes",
----- produces as many POST options as are passes supplied.
getPasses = matchFlag "-p" ("allpasses","yes") $
            map (\p -> ("tracer",p)) . splitOn ","

--------------------------------------------------
-- MAIN IO ---------------------------------------
--------------------------------------------------

----- Keeps retrieving lines until an empty one is encountered
getCode :: IO [String]
getCode = do
  l <- liftM words getLine
  case l of
    [] -> return []
    _ -> do rest <- getCode
            return (l ++ rest)

----- Prints usage
usage :: IO ()
usage = do
  mapM putStrLn
    [ "compiler OPTION* <week>"
    , ""
    , "Where OPTION is one of the following:"
    , "-s <starting-pass>"
    , "\tdefine starting pass."
    , "-p <pass0>,<pass1>,..."
    , "\tdefine passes to trace. (Passes must be comma separated)"
    , "-h"
    , "\tprint this help."
    ]
  return ()

----- The program driver
main :: IO ()
main = do
  args <- getArgs
  if' (elem "-h" args)
    -- User wants help
    usage
    -- Bad week argument
    (if' (or [null args,any (not . isDigit) $ last args])
      (do putStrLn "Please give week number."
          return ())
      -- everything's fine, let's go.
      (do let w = last args;
          home <- getEnv "HOME"
          -- read in authentication
          auth <- liftM (Main.dropWhileEnd isSpace) $ readFile (home ++ "/" ++ authFile)
          -- prompt
          putStrLn "Code to compile: "
          -- read in code
          body <- liftM unwords getCode
          -- call out to curl
          out <- readProcess "curl"
                   [ "-u",auth
                   , "-d", formatData body $ parsePostArgs w args,cgiSite
                   , "-s"
                   ]
                   []
          -- format and print result
          putStrLn $ cleanOutput out))

--------------------------------------------------
-- HELPERS ---------------------------------------
--------------------------------------------------

----- if expression
if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

----- The rest of these are replacing a small set of functionality of
----- The Data.Text module, in order to reduce dependencies.

----- Split String at first occurrence of substring
splitOnce :: String -> String -> (String,String)
splitOnce s l = case l of
  [] -> ("","")
  _ | isPrefixOf s l -> ("",drop (length s) l)
    | otherwise -> ((head l):rest,l')
                   where (rest,l') = splitOnce s $ tail l

----- Split String at all occurrences of substring
splitOn :: String -> String -> [String]
splitOn s l = case l of
  [] -> []
  ls -> (x1:rest)
        where (x1,xs) = splitOnce s l
              rest = splitOn s xs

----- replace one substring with another in a String
replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old

replaceAll :: [(String,String)] -> String -> String
replaceAll rs e = case rs of
  [] -> e
  ((o,n):rs') -> replaceAll rs' $ replace o n e

----- perform replacement with each substring pair provided.
replaceChars :: [(Char,String)] -> String -> String
replaceChars rs e = case e of
  [] -> ""
  (c:e') -> (c'++replaceChars rs e')
            where c' = walkChar c rs

walkChar :: Eq a => a -> [(a,[a])] -> [a]
walkChar x l = case (assoc x l) of
  Nothing -> [x]
  Just (a,b) -> b

assoc :: Eq a => a -> [(a,b)] -> Maybe (a,b)
assoc x l = case l of
  [] -> Nothing
  (p@(k,v):l') -> if' (k == x)
                      (Just p)
                      (assoc x l')

dropWhileEnd f = reverse . dropWhile f . reverse