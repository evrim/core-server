module Main
    where
 
import Text.XML.HXT.Arrow
 
import System.IO
import System.Environment
-- import System.Console.GetOpt
import System.Exit

---
--- Usage: xmltr <id> <input> <output>
---
main :: IO ()
main
    = do
      hSetBuffering stdin NoBuffering  
      argv <- getArgs
      text <- getContents
      (al, id, src, dst) <- cmdlineOpts argv
      [rc]  <- runX (application al id src dst text)
      if rc >= c_err
         then exitWith (ExitFailure (0-1))
         else exitWith ExitSuccess

-- | the dummy for the boring stuff of option evaluation,
-- usually done with 'System.Console.GetOpt'
 
cmdlineOpts :: [String] -> IO (Attributes, String, String, String)
cmdlineOpts argv
    = return ([(a_validate, v_0)], argv!!0, argv!!1, argv!!2)
 
-- | the main arrow

application:: Attributes -> String -> String -> String -> String -> IOSArrow b Int
application al id src dst text
    = readDocument ([(a_parse_html, v_1)]) src
      >>>
      processChildren (addContent id text `when` isElem)
      >>>
      writeDocument [(a_output_html, v_1), (a_indent, v_1), (a_remove_whitespace, v_1), (a_output_encoding, utf8)] dst
      >>>
      getErrStatus

-- | the dummy for the real processing: the identity filter

addContent:: String -> String -> IOSArrow XmlTree XmlTree 
addContent id text =
    processTopDown (insertContent `when` isContent)
    where
      isContent
          = isElem 
            >>> 
            hasAttr "id"
            >>> 
            getAttrValue "id"
            >>> 
            isA (\x -> x == id)
      insertContent
          = replaceChildren (readString [(a_parse_html, v_1), (a_encoding, utf8), (a_validate, v_0)] text 
                             >>> 
                             getChildren >>> getChildren)
