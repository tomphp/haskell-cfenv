# Haskell CFEnv

[![Build Status](https://travis-ci.org/tomphp/haskell-cfenv.svg?branch=master)](https://travis-ci.org/tomphp/haskell-cfenv)

A port of [go-cfenv](https://github.com/cloudfoundry-community/go-cfenv) for
Haskell.

The purpose of this library is to assist you in writing Haskell apps that run
on Cloud Foundry. It provides convenience functions and structures that map to
Cloud Foundry environment variable primitives.

## Progress

**Current State: Early WIP**

- [x] Read environment variables into `Application`
- [x] Read basic values from `VCAP_APPLICATION` into `Application`
- [x] Read application URIs into `Application`
- [x] Read limits into `Application`
- [x] Read `VCAP_SERVICES`
- [ ] Find services by pattern matching
- [ ] Cases from go-cfenv around handling missing data

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.String (fromString)
import Data.Monoid (mconcat)

import Web.Scotty

import qualified System.CloudFoundry.Environment as CfEnv

main = do
  app <- CfEnv.current
  
  scotty (CfEnv.port app) $
    get "/" $ do
      html $ mconcat ["<pre>", (fromString (show app)), "</pre>"] 
```