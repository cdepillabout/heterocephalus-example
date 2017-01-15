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
<h2>variable interpolation</h2>
<p>#{a}</p>

<h2>if control statement</h2>
%{ if (b == 3) }
  <p>b is 3</p>
%{ else }
  <p>b is some other number</p>
%{ endif }

<h2>forall control statement</h2>
<ul>
  %{ forall c <- cs }
  <li>#{c}</li>
  %{ endforall }
</ul>
   |]

renderedTemplate :: String
renderedTemplate = renderHtml template

main :: IO ()
main = putStr renderedTemplate
