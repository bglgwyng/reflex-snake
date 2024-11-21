module Reflex.Utils (networkReset) where

import Reflex
import Reflex.Network

networkReset ::
  forall t m a.
  (Adjustable t m, MonadHold t m) =>
  m a ->
  Event t () ->
  m (Dynamic t a)
networkReset m e = networkHold m (e `ffor` const m)