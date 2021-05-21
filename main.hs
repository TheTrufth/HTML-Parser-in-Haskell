import Data.List

plainTags = ["html", "head", "body", "title", "h1", "h2", "h3", "p", "ul", "li", "a", "div"]
validTags = ["<" ++ c ++ ">" | c <- plainTags] ++ ["</" ++ c ++ ">" | c <- plainTags] ++ ["<br>"] ++ ["<hr>"]



{- Stack implementation using lists -}
empty = []
push x xs = xs ++ [x]
pop [] = []
pop xs = init xs
peek [] = []
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
        if check 0 0 0 extractedTags [] == [] 
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



{- 
identCheck (x:xs) myStack = do
    let myStack = auxIdent x myStack
      if (tag == "<br>" || tag == "<hr>")
      then check tags stack
      else
    print myStack -}

checkRule numOfHtml numOfHead numOfBody tag tags stack = case tag of
  "<html>" -> if numOfHtml == 0 then check 1 numOfHead numOfBody tags (push tag stack) else error "There are multiple html tags!"
  "<head>" -> if numOfHead == 0 then check numOfHtml 1 numOfBody tags (push tag stack) else error "There are multiple head tags!"
  "<br>" -> check numOfHtml numOfHead numOfBody tags stack
  "<hr>" -> check numOfHtml numOfHead numOfBody tags stack
  "<div>" -> if peek stack == "<p>" then error "A <div> tag cannot be nested inside a <p> tag." else check numOfHtml numOfHead numOfBody tags (push tag stack)
  "<p>" -> if peek stack == "<p>" then error "A <p> tag cannot be nested inside a <p> tag." else check numOfHtml numOfHead numOfBody tags (push tag stack)
  "<title>" -> if peek stack == "<head>" then check numOfHtml numOfHead numOfBody tags (push tag stack) else error "<title> tag is not in the <head> tag."
  "<body>" -> if numOfHead == 1 then if numOfBody == 0 then check numOfHtml numOfHead 1 tags (push tag stack) else error "There are multiple <body> tags." else error "<body> tag does not come after <head> tag."
  _ -> check numOfHtml numOfHead numOfBody tags (push tag stack)


check numOfHtml numOfHead numOfBody (tag:tags) stack = do
  if tag == closeTag tag 
      then
      if peek stack == openTag tag then
          if tags /= []
              then check numOfHtml numOfHead numOfBody tags (pop stack)
              else if (numOfHead == 1 && numOfBody == 1 && numOfHtml == 1) then pop stack else if numOfBody /= 1 then error "There are no <body> tags in the document" else error "There are no <html> tags in the document"
      else error (peek stack ++ " AND " ++ tag ++ " NOT NESTED CORRECTLY")
      else
      if tag == openTag tag
          then
              if (tags /= [])
                  then checkRule numOfHtml numOfHead numOfBody tag tags stack
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


