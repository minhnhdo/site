{-# LANGUAGE OverloadedStrings #-}

module SiteCss (siteCss) where

import Prelude hiding ((**), div, span)
import Data.Monoid ((<>))
import Clay

siteCss :: Css
siteCss =
  let sBase03   = rgb 0x00 0x2b 0x36
      sBase02   = rgb 0x07 0x36 0x42
      sBase01   = rgb 0x58 0x6e 0x75
      sBase00   = rgb 0x65 0x7b 0x83
      sBase0    = rgb 0x83 0x94 0x96
      sBase1    = rgb 0x93 0xa1 0xa1
      sBase2    = rgb 0xee 0xe8 0xd5
      sBase3    = rgb 0xfd 0xf6 0xe3
      sYellow   = rgb 0xb5 0x89 0x00
      sOrange   = rgb 0xcb 0x4b 0x16
      sRed      = rgb 0xdc 0x32 0x2f
      sMagenta  = rgb 0xd3 0x36 0x82
      sViolet   = rgb 0x6c 0x71 0xc4
      sBlue     = rgb 0x26 0x8b 0xd2
      sCyan     = rgb 0x2a 0xa1 0x98
      sGreen    = rgb 0x85 0x99 0x00
      pageWidth = pct 90
  in do
    html ? do
      color sBase02
      fontFamily ["Helvetica", "Arial"] [sansSerif]
    star ? color sBase02
    h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
      color sBase03
      borderColor sBase03
      fontWeight bold
    a ? color sBase02
    a # visited ? color sBase01
    a # active <> a # hover ? color sCyan
    h1 # "#pagetitle" <> h1 # firstOfType ? do
      fontSize (px 30)
      marginTop (px 0)
      marginBottom (px 20)
      lineHeight (pct 100)
    body ? do
      width pageWidth
      fontSize (px 16)
      sym2 margin (px 0) auto
    header ? do
      sym2 margin (px 10) auto
      sym2 padding (px 10) 0
      borderBottom solid (px 1) sBase00
      a <> a # visited ? do
        color sBase03
        fontWeight bold
        textDecoration none
      a # active <> a # hover ? color sBlue
      div # "#logo" ? do
        a ? fontSize (px 36)
      div # "#navigation" ? do
        a ? do
          fontSize (px 18)
          marginRight (px 18)
    article ? do
      sym2 margin (px 0) auto
      sym2 padding (px 30) 0
    footer ? do
      color sBase01
      fontSize (px 12)
      textAlign end
      sym2 margin (px 5) auto
    h1 ? fontSize (px 24)
    h2 ? fontSize (px 20)
    div # ".info" <> div # ".tags" ? do
      color sBase01
      fontSize (px 14)
      fontStyle italic
      clear both
      li ? float floatLeft
    pre # ".sourceCode" <> table # ".sourceCode" ? do
      fontFamily ["Monaco", "Inconsolata", "DejaVu Sans Mono", "Courier New", "Courier"] [monospace]
      overflow auto
      table # ".sourceCode" <> star ? do
        fontFamily ["Monaco", "Inconsolata", "DejaVu Sans Mono", "Courier New", "Courier"] [monospace]
    code # ".sourceCode" ? do
      span # ".al" ? do -- Alert
        color (rgb 0xff 0x00 0x00)
        fontWeight bold
      span # ".an" ? do -- Annotation
        color (rgb 0x60 0xa0 0xb0)
        fontWeight bold
        fontStyle italic
      span # ".at" ? do -- Attribute
        color (rgb 0x7d 0x90 0x29)
      span # ".bn" ? do -- BaseN
        color (rgb 0x40 0xa0 0x70)
      -- span # ".bu" -- BuiltIn
      span # ".cf" ? do -- ControlFlow
        color (rgb 0x00 0x70 0x20)
        fontWeight bold
      span # ".ch" ? do -- Char
        color (rgb 0x40 0x70 0xa0)
      span # ".cn" ? do -- Constant
        color (rgb 0x88 0x00 0x00)
      span # ".co" ? do -- Comment
        color (rgb 0x60 0xa0 0xb0)
        fontStyle italic
      span # ".cv" ? do -- CommentVar
        color (rgb 0x60 0xa0 0xb0)
        fontWeight bold
        fontStyle italic
      span # ".do" ? do -- Documentation
        color (rgb 0xba 0x21 0x21)
        fontStyle italic
      span # ".dt" ? do -- DataType
        color (rgb 0x90 0x20 0x00)
      span # ".dv" ? do -- DecVal
        color (rgb 0x40 0xa0 0x70)
      span # ".er" ? do -- Error
        color (rgb 0xff 0x00 0x00)
        fontWeight bold
      -- span # ".ex" -- Extension
      span # ".fl" ? do -- Float
        color (rgb 0x40 0xa0 0x70)
      span # ".fu" ? do -- Function
        color (rgb 0x06 0x28 0x7e)
      -- span # ".im" -- Import
      span # ".in" ? do -- Information
        color (rgb 0x60 0xa0 0xb0)
        fontWeight bold
        fontStyle italic
      span # ".kw" ? do -- Keyword
        color (rgb 0x00 0x70 0x20)
        fontWeight bold
      span # ".op" ? do -- Operator
        color (rgb 0x66 0x66 0x66)
      span # ".ot" ? do -- Other
        color (rgb 0x00 0x70 0x20)
      span # ".pp" ? do -- Preprocessor
        color (rgb 0xbc 0x7a 0x00)
      span # ".sc" ? do -- SpecialChar
        color (rgb 0x40 0x70 0xa0)
      span # ".ss" ? do -- SpecialString
        color (rgb 0xbb 0x66 0x88)
      span # ".st" ? do -- String
        color (rgb 0x40 0x70 0xa0)
      span # ".va" ? do -- Variable
        color (rgb 0x19 0x17 0x7c)
      span # ".vs" ? do -- VerbatimString
        color (rgb 0x40 0x70 0xa0)
      span # ".wa" ? do -- Warning
        color (rgb 0x60 0xa0 0xb0)
        fontWeight bold
        fontStyle italic
