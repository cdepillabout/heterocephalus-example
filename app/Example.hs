{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Text.Heterocephalus (compileText)

template :: Html
template =
  let a = "hello"
      b = 3 :: Int
      cs = ["foo", "bar", "baz"]
  in [compileText|
variable interpolation:
  #{a}

if control statement:
%{ if (b == 3) }
  b is 3
%{ else }
  b is some other number
%{ endif }

forall control statement:
%{ forall c <- cs }
  #{c}
%{ endforall }
   |]

renderedTemplate :: String
renderedTemplate = renderHtml template

main :: IO ()
main = putStr renderedTemplate
