import Data.List

plainTags = ["html", "head", "body", "title", "h1", "h2", "h3", "p", "ul", "li", "a", "div"]
validTags = ["<" ++ c ++ ">" | c <- plainTags] ++ ["</" ++ c ++ ">" | c <- plainTags] ++ ["<br>"] ++ ["<hr>"]



{- Stack implementation using lists -}
empty = []
push x xs = xs ++ [x]
pop xs = init xs
peek s = last s


main = do
    input <- readFile "file.html"
    
    {- This part just extracts the tags from the html file and puts it into a list-}
    let extractedTags = map ('<':) (splitIntoTags input)
    let firstPass = map clearAttributes extractedTags
    let extractedTags = [if elem c validTags then c else " "| c <- firstPass]
    
    if countInvalidTags extractedTags > 0
        then error "There are invalid tags in the HTML Document"
    else
        if check extractedTags [] == [] 
            then print "Parsed Succesfully"
            else print extractedTags



auxIdent tag stack = do
  if tag == closeTag tag
    then
      if peek stack == openTag tag
        then pop stack
        else error "NOT PROPERLY IDENTENTED"
    else
      if tag == openTag tag
        then push tag stack
        else error "THERE ARE INVALID TAGS" 



identCheck (x:xs) myStack = do
    let myStack = auxIdent x myStack
    print myStack


check (tag:tags) stack = do
  if (tag == "<br>" || tag == "<hr>")
      then check tags stack
      else
        if tag == closeTag tag 
            then
            if peek stack == openTag tag then
                if tags /= []
                    then check tags (pop stack)
                    else pop stack
            else error "NOT PROPERLY IDENTENTED"
            else
            if tag == openTag tag
                then
                    if (tags /= [])
                        then check tags (push tag stack)
                        else push tag stack
            else error "THERE ARE INVALID TAGS" 
   

    



{- Referenced from: https://www.haskell.org/onlinereport/standard-prelude.html -}
splitIntoTags s =  case dropWhile (=='<') s of
                      "" -> []
                      s' -> w : splitIntoTags s''
                        where (w, s'') = break (=='<') s'

countInvalidTags tags = length $ filter (== " ") tags

clearAttributes (x:xs) = if x == '<' then "<" ++ takeWhile (`notElem` "> ") xs ++ ">"
             else clearAttributes xs


{- Returns the opening tag of a closing tag -}
openTag (x:y:xs) | x == '<' && y /= '/' = x:y:xs
                | x == '<' && y == '/' = x:xs

{- Returns the closing tag of an opening tag -}
closeTag (x:y:xs) | x == '<' && y /= '/' = "</" ++ y:xs
                | x == '<' && y == '/' = x:y:xs
                | x /= '<' = x:xs


