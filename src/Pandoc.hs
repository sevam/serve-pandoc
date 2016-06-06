{-# LANGUAGE OverloadedStrings          #-}

module Pandoc
    ( convertHtmlToPdf
    ) where

import Text.Pandoc
import Text.Pandoc.Error (PandocError (..), handleError)
import Text.Pandoc.PDF
import Data.ByteString.Lazy.Internal

--------------------------------------------------------------------------------

convertHtmlToPdf :: String -> String -> IO (Either ByteString (Either ByteString ByteString))
convertHtmlToPdf html template =
  do
    let pndcE  = readHtml def html
    case pndcE of
      Left  err  -> return $ Left "Can't read html!"
      Right pndc ->
        do
          let opts =  def { writerTemplate  = template
                          , writerHighlight = False                   
                          }
          pdfE <- makePDF "pdflatex" writeLaTeX opts pndc
          
          return $ Right pdfE
