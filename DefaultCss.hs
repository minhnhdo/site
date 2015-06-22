{-# LANGUAGE OverloadedStrings #-}

module DefaultCss (defaultCss) where

import Prelude hiding ((**), div, span)
import Data.Monoid ((<>))
import Clay

defaultCss :: Css
defaultCss =
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
      pageWidth = px 960
      vMargin   = px 20
  in do
    html ? do
      backgroundColor sBase3
      color sBase01
      fontFamily ["Helvetica", "Arial"] [sansSerif]
    star ? color sBase01
    h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
      color sBase01
      borderColor sBase01
      fontWeight bold
    a <> a # active <> a # visited ? color (rgb 0x60 0x78 0x90)
    a # hover ? color (rgb 0x00 0x33 0x66)
    h1 # "#pagetitle" <> h1 # firstOfType ? do
      fontSize (pct 300)
      marginTop (px 0)
      marginBottom vMargin
      lineHeight (pct 100)
    body ? do
      fontSize (px 16)
      sym2 margin (px 0) auto
    div # "#header-strip" ? do
      sym2 padding (px 10) 0
      backgroundColor sBlue
      overflow auto
    div # "#header" ? do
      width pageWidth
      sym2 margin (px 0) auto
    div # "#logo" ? do
      float floatLeft
      a ? do
        color sBase2
        fontSize (px 24)
        fontWeight bold
        textDecoration none
      span ? do
        color sBase2
        fontSize (px 16)
        fontStyle italic
    div # "#header" ** "#navigation" ? do
      textAlign end
      a ? do
        color sBase2
        fontSize (px 22)
        fontWeight bold
        marginLeft (px 18)
        textDecoration none
    div # "#container" ? do
      width pageWidth
      sym2 margin (px 0) auto
      sym2 padding (px 30) 0
    div # "#content" ? sym2 padding 0 (px 10)
    div # "#footer-strip" ? do
      overflow auto
      sym2 padding (px 12) 0
      -- backgroundColor sBlue
    div # "#footer" ? do
      color sBase1
      fontSize (px 12)
      textAlign end
      width pageWidth
      sym2 margin (px 0) auto
    h1 ? fontSize (px 24)
    h2 ? fontSize (px 20)
    div # ".info" <> div # ".tags" ? do
      color sBase1
      fontSize (px 14)
      fontStyle italic
      clear both
      li ? float floatLeft
    pre # ".sourceCode" <> table # ".sourceCode" ? do
      fontFamily ["Monaco", "Inconsolata", "DejaVu Sans Mono", "Courier New", "Courier"] [monospace]
      table # ".sourceCode" <> star ? do
        fontFamily ["Monaco", "Inconsolata", "DejaVu Sans Mono", "Courier New", "Courier"] [monospace]
    code # ".sourceCode" ? do
      span # ".kw" ? do
        color sYellow
        fontWeight bold
      span # ".dt" ? color sCyan
      span # ".dv" ? color sBase01
      span # ".bn" ? color sOrange
      span # ".fl" ? color sCyan
      span # ".ch" ? color sRed
      span # ".st" ? color sMagenta
      span # ".co" ? do
        color sBase01
        fontStyle italic
      -- span # ".ot" ?
      span # ".al" ? do
        color sGreen
        fontWeight bold
      span # ".fu" ? color sBlue
      -- span # ".re" ?
      span # ".er" ? do
        color sRed
        fontWeight bold
