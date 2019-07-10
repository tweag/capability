{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
module Capability.Stream {-# DEPRECATED "Use 'Capability.Sink' "#-}
  ( -- * Interface
    HasStream
  , HasStream'
  , yield
    -- * Strategies
  , StreamStack
  , StreamDList
  , StreamLog
    -- ** Modifiers
  , module Capability.Accessors
  ) where

import Capability.Accessors

import Capability.Sink

{-# DEPRECATED HasStream "Use 'HasSink'" #-}
type HasStream = HasSink

{-# DEPRECATED StreamStack "Use 'SinkStack'" #-}
type StreamStack = SinkStack

{-# DEPRECATED StreamDList "Use 'SinkDList'" #-}
type StreamDList = SinkDList

{-# DEPRECATED StreamLog "Use 'SinkLog'" #-}
type StreamLog = SinkLog

{-# DEPRECATED HasStream' "Use 'HasSink''" #-}
type HasStream' tag = HasSink' tag
